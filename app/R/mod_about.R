# -----------------------------------------------------------------------------
# Under the hood: storage architecture, live query log, SQL console, and the
# list of data problems deliberately injected into the simulation.
# -----------------------------------------------------------------------------

SQL_EXAMPLES <- list(
  "Top métiers" = "SELECT MetierL6, round(sum(FishingHour)) AS hours, round(sum(TotWeight)/1000) AS tonnes\nFROM vms WHERE Year = 2024\nGROUP BY ALL ORDER BY hours DESC LIMIT 15",
  "Partition pruning" = "EXPLAIN ANALYZE\nSELECT count(*), sum(FishingHour) FROM vms\nWHERE Year = 2022 AND CountryCode = 'NL'\n  AND lat BETWEEN 54 AND 56 AND lon BETWEEN 1 AND 4",
  "Cube vs lake" = "-- the cube answers the same question from ~100x fewer rows\nSELECT yr, sum(h) AS hours FROM cube_20 WHERE g = 'TBB' GROUP BY yr ORDER BY yr",
  "Rows per partition" = "SELECT Year, count(*) AS rows, count(DISTINCT Csquare) AS csquares\nFROM vms GROUP BY Year ORDER BY Year",
  "Logbook vs VMS" = "SELECT l.Year, round(sum(l.TotWeight)/1e6, 1) AS logbook_kt,\n       round((SELECT sum(TotWeight) FROM vms v WHERE v.Year = l.Year)/1e6, 1) AS vms_kt\nFROM logbook l WHERE VMSEnabled = 'Y' GROUP BY l.Year ORDER BY 1"
)

about_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header(bsicons::bs_icon("diagram-3"), " How the data is stored and served"),
      div(class = "arch",
        div(class = "box",
          tags$h6("Cold · Parquet lake"),
          p("Full ICES Table 1 (VMS, c-square) and Table 2 (logbook, rectangle) for every country and year, as delivered."),
          tags$ul(class = "small mb-0",
            tags$li(tags$code("lake/vms/Year=YYYY/CountryCode=XX/"), " hive partitions = one folder per national submission → year / country filters skip whole files; a resubmission replaces one folder"),
            tags$li("rows Z-order (Morton) sorted on the c-square grid, 8k-row groups → bounding-box queries read only overlapping row groups"),
            tags$li("zstd + dictionary encoding; grid indices & centroids materialised at ingest"))),
        div(class = "arrow", "→"),
        div(class = "box",
          tags$h6("Hot · DuckDB cubes"),
          p("Built once from the lake (", tags$code("build_warehouse.R"), ")."),
          tags$ul(class = "small mb-0",
            tags$li("pyramid at 0.05° / 0.25° / 0.5° / 1° (0.1° rolled up on the fly for the viewport); the map picks the level whose cells are a few pixels wide"),
            tags$li("dims stored as ENUMs (1 byte); habitat/depth/L6 dropped"),
            tags$li(tags$code("cube_ts"), " (no space) for charts, ", tags$code("le_cube"), " + ", tags$code("vms_rect"), " for reconciliation"))),
        div(class = "arrow", "→"),
        div(class = "box",
          tags$h6("Browser · canvas renderer"),
          p("Shiny sends base64 typed arrays (Int16 x/y, Float32 value), not GeoJSON."),
          tags$ul(class = "small mb-0",
            tags$li("one canvas, cells batched by colour → 100k+ cells per frame"),
            tags$li("palette, scale & opacity applied client-side — zero round-trips"),
            tags$li("time-lapse frames fetched in one query and animated locally"),
            tags$li("LRU result cache keyed on SQL; viewport reuse at fine levels"))))
    ),
    layout_columns(
      col_widths = c(5, 7), row_heights = "420px",
      card(card_header("Storage footprint"), DT::DTOutput(ns("storage"))),
      card(card_header(class = "d-flex justify-content-between", span(bsicons::bs_icon("speedometer2"), " Live query log"),
                       actionLink(ns("clear_cache"), "clear cache")),
           DT::DTOutput(ns("qlog")))
    ),
    if (SQL_CONSOLE) card(
      full_screen = TRUE,
      card_header(bsicons::bs_icon("terminal"), " SQL console · read-only DuckDB (views: ", tags$code("vms"), ", ", tags$code("logbook"),
                  "; tables: ", tags$code("cube_1 … cube_20, cube_ts, le_cube, vms_rect, dim_metier"), ")"),
      div(lapply(names(SQL_EXAMPLES), function(n) tags$span(class = "badge text-bg-secondary sql-chip", `data-sql` = SQL_EXAMPLES[[n]], n))),
      textAreaInput(ns("sql_text"), NULL, value = SQL_EXAMPLES[[1]], rows = 5, width = "100%"),
      div(class = "d-flex gap-2 align-items-center",
          actionButton(ns("run_sql"), "Run", icon = icon("play"), class = "btn-sm btn-primary"),
          uiOutput(ns("sql_status"), inline = TRUE)),
      DT::DTOutput(ns("sql_out")),
      tags$script(HTML(sprintf("document.addEventListener('click', e => { const c = e.target.closest('.sql-chip'); if (!c) return;
        const t = document.getElementById('%s'); t.value = c.dataset.sql; t.dispatchEvent(new Event('change')); });", ns("sql_text"))))
    ),
    card(card_header(bsicons::bs_icon("bug"), " Data problems injected into the simulation (the QC Lab should find these)"),
         DT::DTOutput(ns("issues")))
  )
}

about_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tick <- reactiveTimer(2000)

    output$storage <- DT::renderDT({
      files <- list.files(file.path(DATA_DIR, "lake"), recursive = TRUE, full.names = TRUE, pattern = "parquet$")
      sz <- function(p) sum(file.size(p))
      ls <- q("SELECT tbl, sum(rows) AS rows, count(*) AS parts FROM lake_stats GROUP BY tbl", "storage stats")
      cubes <- q("SELECT table_name, estimated_size AS rows FROM duckdb_tables() WHERE database_name = 'wh' ORDER BY table_name", "storage stats")
      d <- data.frame(
        Object = c("lake/vms (Table 1)", "lake/logbook (Table 2)", "warehouse.duckdb", paste0("  ", cubes$table_name)),
        Rows = c(ls$rows[ls$tbl == "vms"], ls$rows[ls$tbl == "logbook"], NA, cubes$rows),
        `Size (MB)` = c(round(sz(grep("/vms/", files, value = TRUE)) / 1e6, 1), round(sz(grep("/logbook/", files, value = TRUE)) / 1e6, 1),
                        round(file.size(file.path(DATA_DIR, "warehouse.duckdb")) / 1e6, 1), rep(NA, nrow(cubes))),
        check.names = FALSE)
      DT::datatable(d, rownames = FALSE, selection = "none", options = list(dom = "t", pageLength = 30, ordering = FALSE)) |>
        DT::formatRound("Rows", 0)
    })

    output$qlog <- DT::renderDT({
      tick()
      input$clear_cache
      rows <- QLOG$rows
      if (!length(rows)) return(NULL)
      d <- do.call(rbind, lapply(rows, function(r) data.frame(Time = r$time, Query = r$label, ms = r$ms, Rows = r$rows,
                                                                Source = if (r$cached) "cache" else "duckdb")))
      DT::datatable(d, rownames = FALSE, selection = "none", options = list(dom = "tp", pageLength = 10, ordering = FALSE)) |>
        DT::formatStyle("Source", color = DT::styleEqual(c("cache", "duckdb"), c("#2fb67c", "inherit")), fontWeight = "bold") |>
        DT::formatStyle("ms", background = DT::styleColorBar(c(0, 500), "rgba(18,164,180,.3)"), backgroundSize = "98% 70%",
                        backgroundRepeat = "no-repeat", backgroundPosition = "center")
    })
    observeEvent(input$clear_cache, { QCACHE$reset(); showNotification("Result cache cleared", type = "message") })

    sql_res <- eventReactive(input$run_sql, {
      txt <- trimws(input$sql_text)
      t0 <- proc.time()[[3]]
      out <- tryCatch({
        if (!grepl("^(select|with|explain|describe|show|summarize|pivot|unpivot|from|values)\\b", tolower(txt)))
          stop("Only read queries (SELECT / WITH / EXPLAIN / DESCRIBE / SUMMARIZE / FROM) are allowed.")
        d <- DBI::dbGetQuery(CON, txt, n = 5000)
        list(d = d, err = NULL)
      }, error = function(e) list(d = NULL, err = conditionMessage(e)))
      out$ms <- (proc.time()[[3]] - t0) * 1000
      log_query("SQL console", out$ms, if (is.null(out$d)) 0 else nrow(out$d), FALSE, txt)
      out
    })
    output$sql_status <- renderUI({
      r <- sql_res()
      if (!is.null(r$err)) span(class = "text-danger small mono", r$err)
      else span(class = "small mono text-muted", sprintf("%s rows · %.0f ms", format(nrow(r$d), big.mark = ","), r$ms))
    })
    output$sql_out <- DT::renderDT({
      r <- sql_res(); req(is.null(r$err))
      d <- r$d
      for (cn in names(d)) if (is.list(d[[cn]])) d[[cn]] <- vapply(d[[cn]], function(x) paste(format(x), collapse = ", "), "")
      DT::datatable(d, rownames = FALSE, selection = "none", options = list(pageLength = 10, scrollX = TRUE, dom = "tip"))
    })

    output$issues <- DT::renderDT({
      ii <- META$injected_issues
      if (!is.data.frame(ii) || !nrow(ii)) ii <- data.frame(note = "Real submission loaded \u2014 no injected issues.")
      DT::datatable(ii, rownames = FALSE, selection = "none", options = list(dom = "t", ordering = FALSE))
    })
  })
}

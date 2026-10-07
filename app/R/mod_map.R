# -----------------------------------------------------------------------------
# Map explorer: adaptive-resolution c-square map, viewport summary, period
# comparison, time-lapse and cell drill-down into the raw lake.
# -----------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

closures_payload <- function() {
  wf <- META$wind_farms; bc <- META$bottom_closures
  rows <- list()
  if (is.matrix(wf)) for (i in seq_len(nrow(wf))) rows[[length(rows) + 1]] <- c(as.list(wf[i, ]), "wind")
  if (is.matrix(bc)) for (i in seq_len(nrow(bc))) rows[[length(rows) + 1]] <- c(as.list(bc[i, ]), "closure")
  rows
}

# CARTO basemaps require a free API key (carto.com/basemaps/apikey). It is read
# from the environment so it is not committed; without it tiles are watermarked.
CARTO_KEY <- Sys.getenv("CARTO_BASEMAP_KEY")
if (!nzchar(CARTO_KEY)) {
  warning("CARTO_BASEMAP_KEY is not set; CARTO basemap tiles will be watermarked.", call. = FALSE)
}

carto_tiles <- function(map, style, group) {
  leaflet::addTiles(
    map,
    urlTemplate = sprintf("https://{s}.basemaps.cartocdn.com/rastertiles/%s/{z}/{x}/{y}{r}.png?key=%s",
                          style, utils::URLencode(CARTO_KEY, reserved = TRUE)),
    attribution = "&copy; OpenStreetMap contributors &copy; CARTO",
    options = leaflet::tileOptions(subdomains = "abcd"),
    group = group
  )
}

base_map <- function(lng = 2, lat = 60, zoom = 4) {
  leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 3, maxZoom = 11, worldCopyJump = FALSE, attributionControl = TRUE)) |>
    carto_tiles("dark_nolabels", "Dark") |>
    leaflet::addProviderTiles("Esri.OceanBasemap", group = "Ocean") |>
    carto_tiles("light_nolabels", "Light") |>
    leaflet::addLayersControl(baseGroups = c("Dark", "Ocean", "Light"), position = "topright",
                              options = leaflet::layersControlOptions(collapsed = TRUE)) |>
    leaflet::setView(lng, lat, zoom)
}

perf_push <- function(session, res, label) {
  ms <- attr(res, "ms") %||% 0
  hit <- isTRUE(attr(res, "cached"))
  tot <- QLOG$hits + QLOG$miss
  html <- sprintf("&#9889; %s %s &middot; cache %d/%d", label,
                  if (hit) "<span class='hit'>hit</span>" else sprintf("%.0f ms", ms), QLOG$hits, tot)
  session$sendCustomMessage("perf", list(html = html))
}

send_grid <- function(session, id, df, x0, y0, dx, dy, kind, m, mode = "seq", center = 0, opts = list(), unit = NULL, label = NULL) {
  ix <- b64_i16(df$x); iy <- b64_i16(df$y); v <- b64_f32(df$v)
  session$sendCustomMessage("csq-data", list(
    id = id, opts = opts, x0 = x0, y0 = y0, dx = dx, dy = dy, kind = kind,
    ix = ix, iy = iy, v = v,
    label = label %||% METRICS[[m]]$label, unit = unit %||% METRICS[[m]]$unit, mode = mode, center = center,
    meta = list(ms = attr(df, "ms"), cached = isTRUE(attr(df, "cached")),
                kb = round((nchar(ix) + nchar(iy) + nchar(v)) / 1024))
  ))
}

map_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    fillable = TRUE, border = FALSE, padding = 0,
    sidebar = sidebar(
      id = ns("drill_sb"), position = "right", open = FALSE, width = 430,
      uiOutput(ns("drill_head")),
      plotly::plotlyOutput(ns("drill_year"), height = "210px"),
      plotly::plotlyOutput(ns("drill_month"), height = "150px"),
      uiOutput(ns("drill_bars"))
    ),
    div(class = "map-wrap",
        leaflet::leafletOutput(ns("map"), height = "100%"),
        uiOutput(ns("inview"), class = "inview-card"),
        uiOutput(ns("cmpbadge")))
  )
}

map_server <- function(id, f, metric, cmp, dark) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    mapid <- ns("map")
    opts <- list(grid = TRUE, timelapse = TRUE)

    output$map <- leaflet::renderLeaflet(base_map())

    # basemap follows dark mode
    observeEvent(dark(), {
      grp <- if (identical(dark(), "light")) "Ocean" else "Dark"
      leaflet::leafletProxy("map") |> leaflet::hideGroup(c("Dark", "Ocean", "Light")) |> leaflet::showGroup(grp)
    }, ignoreInit = FALSE)

    observeEvent(input$map_zoom, {
      session$sendCustomMessage("csq-init", c(list(id = mapid, closures = closures_payload()), opts))
    }, once = TRUE)

    bounds <- debounce(reactive(input$map_bounds), 200)
    level <- reactive({
      r <- input$map_res %||% "auto"
      if (identical(r, "auto")) level_for_zoom(input$map_zoom) else as.integer(r)
    })

    # ---- grid query ----------------------------------------------------------
    fetch_grid <- function(ff, m, k, bb, cc) {
      cr <- cube_ref(k); tbl <- cr$tbl
      sp <- if (!is.null(bb)) bbox_sql(bb, k)
      if (isTRUE(cc$on)) {
        ya <- ff$years; yb <- cc$years
        fall <- ff; fall$years <- range(c(ya, yb))
        fa <- sprintf("yr BETWEEN %d AND %d", ya[1], ya[2])
        fb <- sprintf("yr BETWEEN %d AND %d", yb[1], yb[2])
        ratio <- isTRUE(METRICS[[m]]$ratio)
        ea <- mexpr(m, "cube", fa); eb <- mexpr(m, "cube", fb)
        if (!ratio) { ea <- sprintf("(%s) / %d", ea, diff(ya) + 1); eb <- sprintf("(%s) / %d", eb, diff(yb) + 1) }
        val <- if (identical(cc$type, "pct"))
          "CASE WHEN a > 0 THEN least(greatest((coalesce(b,0) - a) / a * 100, -100), 400) END"
        else "coalesce(b,0) - coalesce(a,0)"
        sql <- sprintf("SELECT x, y, %s AS v FROM (SELECT %s AS x, %s AS y, %s AS a, %s AS b FROM %s %s GROUP BY 1, 2) WHERE v IS NOT NULL AND v <> 0",
                       val, cr$x, cr$y, ea, eb, tbl, where_sql(fall, extra = sp))
      } else {
        sql <- sprintf("SELECT %s AS x, %s AS y, %s AS v FROM %s %s GROUP BY 1, 2 HAVING v > 0",
                       cr$x, cr$y, mexpr(m), tbl, where_sql(ff, extra = sp))
      }
      q(sql, sprintf("map %s @%.2f°", m, level_deg(k)))
    }

    inside <- function(b, bb) !is.null(bb) && b$west >= bb$west && b$east <= bb$east && b$south >= bb$south && b$north <= bb$north
    loaded <- reactiveVal(NULL)
    reload <- reactiveVal(0)

    observe({
      reload()
      b <- bounds(); req(b, input$map_zoom)
      ff <- f(); m <- metric(); k <- level(); cc <- cmp()
      key <- rlang::hash(list(ff, m, k, cc))
      spatial <- k <= 2L
      cur <- isolate(loaded())
      if (!is.null(cur) && identical(cur$key, key) && (!spatial || inside(b, cur$bb))) return()
      bb <- if (spatial) bbox_cells(b, k, pad = 0.35) else NULL
      df <- fetch_grid(ff, m, k, bb, cc)
      mode <- if (isTRUE(cc$on)) "div" else "seq"
      unit <- if (isTRUE(cc$on) && identical(cc$type, "pct")) "%" else if (isTRUE(cc$on) && !isTRUE(METRICS[[m]]$ratio)) paste0(METRICS[[m]]$unit, "/yr") else NULL
      label <- if (isTRUE(cc$on)) paste0("Δ ", METRICS[[m]]$label) else NULL
      send_grid(session, mapid, df, GRID$lon0, GRID$lat0, level_deg(k), level_deg(k), "csq", m, mode, opts = opts, unit = unit, label = label)
      perf_push(session, df, "map")
      loaded(list(key = key, bb = bb))
    })

    output$cmpbadge <- renderUI({
      cc <- cmp(); ff <- f()
      if (!isTRUE(cc$on)) return(NULL)
      div(class = "compare-badge", sprintf("Δ %s: %d–%d vs %d–%d %s",
          METRICS[[metric()]]$label, cc$years[1], cc$years[2], ff$years[1], ff$years[2],
          if (identical(cc$type, "pct")) "(% change)" else if (isTRUE(METRICS[[metric()]]$ratio)) "" else "(per-year mean)"))
    })

    # ---- viewport summary (one GROUPING SETS query) -----------------------------
    output$inview <- renderUI({
      b <- bounds(); req(b)
      ff <- f(); m <- metric(); k <- level()
      bb <- bbox_cells(b, k, pad = 0)
      cr <- cube_ref(k)
      sql <- sprintf("SELECT g::VARCHAR AS g, c::VARCHAR AS c, %s AS v, count(DISTINCT %s::INTEGER * 100000 + %s) AS cells
                      FROM %s %s GROUP BY GROUPING SETS ((g), (c), ()) ",
                     mexpr(m), cr$x, cr$y, cr$tbl, where_sql(ff, extra = bbox_sql(bb, k)))
      d <- q(sql, "in-view summary")
      tot <- d[is.na(d$g) & is.na(d$c), ]
      gg <- d[!is.na(d$g), ]; gg <- head(gg[order(-gg$v), ], 6)
      cc <- d[!is.na(d$c), ]; cc <- head(cc[order(-cc$v), ], 6)
      u <- METRICS[[m]]$unit
      tagList(
        div(class = "eyebrow", "In view · ", METRICS[[m]]$label),
        div(class = "inview-total", if (nrow(tot)) fmt_num(tot$v, 1) else "–", tags$small(style = "font-size:.7rem;opacity:.6", u)),
        div(style = "font-size:.7rem;opacity:.6", sprintf("%s cells at %.2f° · %d–%d",
            if (nrow(tot)) format(tot$cells, big.mark = ",") else 0, level_deg(k), ff$years[1], ff$years[2])),
        tags$h6("By gear (L4)"), bar_rows(gg$g, gg$v, colours = pal_for(gg$g)),
        tags$h6("By country"), bar_rows(cc$c, cc$v)
      )
    })

    # ---- time-lapse: all frames in one query, animated in the browser ----------
    observeEvent(input$map_timelapse, {
      by <- input$map_timelapse$by
      ff <- f(); m <- metric(); k <- max(level(), 2L)
      sp <- NULL
      if (k <= 2L) { bb <- bbox_cells(input$map_bounds, k, pad = 0.2); sp <- bbox_sql(bb, k) }
      grp <- if (identical(by, "mo")) "mo" else "yr"
      cr <- cube_ref(k)
      sql <- sprintf("SELECT %s AS f, %s AS x, %s AS y, %s AS v FROM %s %s GROUP BY 1, 2, 3 HAVING v > 0 ORDER BY f",
                     grp, cr$x, cr$y, mexpr(m), cr$tbl, where_sql(ff, extra = sp))
      df <- q(sql, sprintf("time-lapse by %s", grp))
      perf_push(session, df, "time-lapse")
      sp_l <- split(df, df$f)
      frames <- lapply(names(sp_l), function(nm) {
        d <- sp_l[[nm]]
        list(label = if (grp == "mo") month.abb[as.integer(nm)] else nm, ix = b64_i16(d$x), iy = b64_i16(d$y), v = b64_f32(d$v))
      })
      session$sendCustomMessage("csq-frames", list(id = mapid, opts = opts, x0 = GRID$lon0, y0 = GRID$lat0,
        dx = level_deg(k), dy = level_deg(k), kind = "csq", frames = frames,
        label = paste(METRICS[[m]]$label, if (grp == "mo") "(seasonal cycle)" else "by year"), unit = METRICS[[m]]$unit))
    })
    observeEvent(input$map_timelapse_close, { loaded(NULL); reload(reload() + 1) })

    # ---- drill-down into the raw lake --------------------------------------------
    drill <- reactive({
      cell <- input$map_cell; req(cell)
      ff <- f(); m <- metric()
      k <- max(1L, as.integer(round(cell$dx / GRID$res)))
      x0 <- cell$ix * k; y0 <- cell$iy * k
      sp <- sprintf("ix BETWEEN %d AND %d AND iy BETWEEN %d AND %d", x0, x0 + k - 1L, y0, y0 + k - 1L)
      sel <- sprintf("Year BETWEEN %d AND %d", ff$years[1], ff$years[2])
      sql <- sprintf("
        SELECT GROUPING(Year, MetierL4, Month, MetierL6, CountryCode, HabitatType, DepthRange) AS gid,
               Year, MetierL4, Month, MetierL6, CountryCode, HabitatType, DepthRange,
               %s AS v_all, %s AS v_sel,
               sum(FishingHour) FILTER (WHERE %s) AS h, sum(TotWeight) FILTER (WHERE %s) AS kg,
               sum(TotValue) FILTER (WHERE %s) AS eur, sum(NumberOfRecords) FILTER (WHERE %s) AS n,
               count(*) FILTER (WHERE %s) AS rows_sel,
               count(*) FILTER (WHERE %s AND NoDistinctVessels < 3) AS rows_conf,
               count(DISTINCT Csquare) FILTER (WHERE %s) AS ncsq,
               any_value(Csquare) AS csq, string_agg(DISTINCT ICESrectangle, ', ') AS rects
        FROM vms %s
        GROUP BY GROUPING SETS ((Year, MetierL4), (Month), (MetierL6), (CountryCode), (HabitatType), (DepthRange), ())",
        mexpr(m, "lake"), mexpr(m, "lake", sel), sel, sel, sel, sel, sel, sel, sel,
        where_sql(ff, "lake", years = FALSE, extra = sp))
      d <- q(sql, "drill-down (lake)")
      perf_push(session, d, "drill-down")
      list(d = d, cell = cell, k = k, m = m, ff = ff)
    })

    observeEvent(input$map_cell, {
      tog <- if (exists("toggle_sidebar", asNamespace("bslib"))) bslib::toggle_sidebar else bslib::sidebar_toggle
      tog("drill_sb", open = TRUE)
    })

    output$drill_head <- renderUI({
      if (is.null(input$map_cell)) return(div(class = "empty-state", bsicons::bs_icon("cursor"), br(), "Click any cell on the map to drill into the raw Table 1 records."))
      r <- drill(); d <- r$d
      tot <- d[d$gid == max(d$gid), ]
      m <- METRICS[[r$m]]
      tagList(
        div(class = "drill-head",
            tags$h5(style = "margin:0", sprintf("%.2f° cell", r$k * GRID$res)),
            actionLink(ns("zoom_cell"), tagList(bsicons::bs_icon("zoom-in"), " zoom"))),
        div(class = "drill-code", sprintf("%.2f–%.2f°N, %.2f–%.2f°E", r$cell$lat0, r$cell$lat0 + r$cell$dy, r$cell$lon0, r$cell$lon0 + r$cell$dx)),
        div(class = "drill-code", if (r$k == 1) paste("C-square", tot$csq) else sprintf("%s c-squares fished", tot$ncsq),
            " · ICES ", tot$rects),
        div(class = "kpi-mini",
            div(div(class = "k", "Fishing hours"), div(class = "v", fmt_num(tot$h, 0))),
            div(div(class = "k", "Landings (t)"), div(class = "v", fmt_num(tot$kg / 1000, 1))),
            div(div(class = "k", "Value (k€)"), div(class = "v", fmt_num(tot$eur / 1000, 1))),
            div(div(class = "k", "Rows < 3 vessels"), div(class = "v", sprintf("%.0f%%", 100 * tot$rows_conf / max(tot$rows_sel, 1))))
        ),
        div(class = "eyebrow mt-2", sprintf("%s by year & gear \u00b7 all years, selection shaded", m$label))
      )
    })

    output$drill_year <- plotly::renderPlotly({
      r <- drill(); d <- r$d
      y <- d[!is.na(d$Year) & !is.na(d$MetierL4), c("Year", "MetierL4", "v_all")]
      req(nrow(y))
      cols <- pal_for(unique(y$MetierL4))
      p <- plotly::plot_ly(y, x = ~Year, y = ~v_all, color = ~MetierL4, colors = cols, type = "bar",
                           hovertemplate = "%{x} %{fullData.name}: %{y:,.1f}<extra></extra>") |>
        plotly::layout(barmode = if (isTRUE(METRICS[[r$m]]$ratio)) "group" else "stack",
          shapes = list(list(type = "rect", xref = "x", yref = "paper", x0 = r$ff$years[1] - 0.5, x1 = r$ff$years[2] + 0.5,
                             y0 = 0, y1 = 1, fillcolor = "rgba(18,164,180,0.10)", line = list(width = 0))))
      p_layout(p, legend = TRUE) |> plotly::layout(xaxis = list(title = "", tickformat = "d"), yaxis = list(title = ""), legend = list(y = -0.25),
                                                   margin = list(l = 40, r = 6, t = 6, b = 30)) |>
        plotly::config(displayModeBar = FALSE)
    })

    output$drill_month <- plotly::renderPlotly({
      r <- drill(); d <- r$d
      mo <- d[!is.na(d$Month), c("Month", "v_sel")]
      req(nrow(mo))
      mo <- mo[order(mo$Month), ]
      p <- plotly::plot_ly(mo, x = ~month.abb[Month], y = ~v_sel, type = "bar", marker = list(color = "#12a4b4"),
                           hovertemplate = "%{x}: %{y:,.1f}<extra></extra>") |>
        plotly::layout(xaxis = list(categoryorder = "array", categoryarray = month.abb))
      p_layout(p, legend = FALSE) |> plotly::layout(xaxis = list(title = ""), yaxis = list(title = ""), margin = list(l = 40, r = 6, t = 6, b = 24)) |>
        plotly::config(displayModeBar = FALSE)
    })

    output$drill_bars <- renderUI({
      r <- drill(); d <- r$d
      top <- function(col, n = 6) { x <- d[!is.na(d[[col]]) & d$gid != max(d$gid), c(col, "v_sel")]; x <- x[!is.na(x$v_sel), ]; head(x[order(-x$v_sel), ], n) }
      l6 <- top("MetierL6"); cc <- top("CountryCode"); hb <- top("HabitatType", 5); dp <- top("DepthRange", 5)
      tagList(
        div(class = "eyebrow mt-2", "Top métiers (L6)"), bar_rows(l6$MetierL6, l6$v_sel, colours = pal_for(sub("_.*", "", l6$MetierL6)), wide = TRUE),
        div(class = "eyebrow mt-2", "Countries"), bar_rows(cc$CountryCode, cc$v_sel),
        div(class = "eyebrow mt-2", "Depth range (m)"), bar_rows(dp$DepthRange, dp$v_sel),
        div(class = "eyebrow mt-2", "MSFD habitat"), bar_rows(hb$HabitatType, hb$v_sel, wide = TRUE),
        div(class = "drill-code mt-2", sprintf("1 scan of the Parquet lake · %.0f ms%s", attr(d, "ms"), if (isTRUE(attr(d, "cached"))) " (cache)" else ""))
      )
    })

    observeEvent(input$zoom_cell, {
      c <- input$map_cell
      session$sendCustomMessage("flyto", list(id = mapid, bounds = list(c(c$lat0, c$lon0), c(c$lat0 + c$dy, c$lon0 + c$dx))))
    })
  })
}

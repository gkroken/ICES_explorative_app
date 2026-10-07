# -----------------------------------------------------------------------------
# VMS <-> logbook reconciliation (Table 1 vs Table 2) at ICES-rectangle level.
# -----------------------------------------------------------------------------

LE_METRICS <- list(
  kg    = list(label = "Logbook landings", unit = "t", sql = "sum(kg)/1000"),
  eur   = list(label = "Logbook value", unit = "k€", sql = "sum(eur)/1000"),
  days  = list(label = "Fishing days", unit = "days", sql = "sum(days)"),
  kwd   = list(label = "kW fishing days", unit = "kW·d", sql = "sum(kwd)"),
  novms = list(label = "Effort not VMS-enabled", unit = "% of days", sql = "100 * sum(days) FILTER (WHERE NOT vms) / sum(days)", scale = "linear"),
  cover = list(label = "VMS ÷ logbook landings", unit = "%", div = 100)
)

logbook_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(8, 4), row_heights = "560px",
      card(full_screen = TRUE, class = "p-0",
        card_header(class = "d-flex justify-content-between align-items-center",
          span(bsicons::bs_icon("grid-3x3"), " Logbook (Table 2) by ICES rectangle"),
          selectInput(ns("le_metric"), NULL, width = "250px",
                      choices = setNames(names(LE_METRICS), vapply(LE_METRICS, `[[`, "", "label")), selected = "cover")),
        card_body(padding = 0, div(class = "map-wrap", style = "min-height:500px", leaflet::leafletOutput(ns("lemap"), height = "100%")))),
      card(full_screen = TRUE, card_header(textOutput(ns("rect_title"), inline = TRUE)),
           plotly::plotlyOutput(ns("rect_ts"), height = "260px"), uiOutput(ns("rect_info")))
    ),
    layout_columns(
      col_widths = c(4, 4, 4), row_heights = "400px",
      card(full_screen = TRUE,
           card_header(tooltip(span("Effort invisible to VMS ", bsicons::bs_icon("info-circle")),
             "Logbook fishing days by vessel length. 'N' = trips with no matching VMS track: vessels under the national VMS threshold (Norway: 15 m, EU: 12 m) or trips that failed to link.")),
           plotly::plotlyOutput(ns("vlen"))),
      card(full_screen = TRUE, card_header("VMS vs logbook landings per rectangle"), plotly::plotlyOutput(ns("scatter"))),
      card(full_screen = TRUE, card_header("Country reconciliation"), DT::DTOutput(ns("ctab")))
    )
  )
}

logbook_server <- function(id, f, dark) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    mapid <- ns("lemap")
    opts <- list(grid = FALSE, timelapse = FALSE)
    output$lemap <- leaflet::renderLeaflet(base_map(-2, 58, 4))
    observeEvent(dark(), {
      grp <- if (identical(dark(), "light")) "Ocean" else "Dark"
      leaflet::leafletProxy("lemap") |> leaflet::hideGroup(c("Dark", "Ocean", "Light")) |> leaflet::showGroup(grp)
    })
    observeEvent(input$lemap_zoom, session$sendCustomMessage("csq-init", c(list(id = mapid, closures = closures_payload()), opts)), once = TRUE)

    observe({
      req(input$lemap_zoom)
      ff <- f(); m <- input$le_metric; lm <- LE_METRICS[[m]]
      if (m == "cover") {
        sql <- sprintf("
          WITH l AS (SELECT rx, ry, sum(kg) FILTER (WHERE vms) AS lk FROM le_cube %s GROUP BY ALL),
               v AS (SELECT rx, ry, sum(kg) AS vk FROM vms_rect %s GROUP BY ALL)
          SELECT l.rx AS x, l.ry AS y, 100 * coalesce(v.vk, 0) / l.lk AS v FROM l LEFT JOIN v USING (rx, ry) WHERE l.lk > 1000",
          where_sql(ff), where_sql(ff))
      } else {
        sql <- sprintf("SELECT rx AS x, ry AS y, %s AS v FROM le_cube %s GROUP BY ALL HAVING v > 0", lm$sql, where_sql(ff))
      }
      df <- q(sql, paste("logbook map", m))
      df <- df[!is.na(df$v), ]
      msg_mode <- if (!is.null(lm$div)) "div" else "seq"
      send_grid(session, mapid, df, RECT$lon0, RECT$lat0, RECT$dx, RECT$dy, "rect", "kg", mode = msg_mode,
                center = lm$div %||% 0, opts = opts, unit = lm$unit, label = lm$label)
      session$sendCustomMessage("csq-style", list(id = mapid, scale = lm$scale %||% "log"))
      perf_push(session, df, "logbook map")
    })

    sel_rect <- reactive(input$lemap_cell$rect)
    output$rect_title <- renderText({
      r <- sel_rect()
      if (is.null(r)) "Click a rectangle on the map" else sprintf("ICES rectangle %s · landings by year", r)
    })
    rect_data <- reactive({
      r <- sel_rect(); req(r)
      ff <- f()
      wl <- where_sql(ff, years = FALSE, extra = sprintf("rect = %s", sq(r)))
      l <- q(sprintf("SELECT yr, sum(kg)/1000 AS le_all, sum(kg) FILTER (WHERE vms)/1000 AS le_vms,
                      sum(days) AS days, sum(days) FILTER (WHERE NOT vms) AS days_novms
                      FROM le_cube %s GROUP BY yr ORDER BY yr", wl), "rect detail: logbook")
      v <- q(sprintf("SELECT yr, sum(kg)/1000 AS vms, sum(h) AS h FROM vms_rect %s GROUP BY yr", wl), "rect detail: vms")
      merge(l, v, by = "yr", all = TRUE)
    })
    output$rect_ts <- plotly::renderPlotly({
      d <- rect_data(); req(nrow(d))
      p <- plotly::plot_ly(d, x = ~yr) |>
        plotly::add_bars(y = ~le_all, name = "Logbook (all)", marker = list(color = "rgba(154,165,177,.45)")) |>
        plotly::add_lines(y = ~le_vms, name = "Logbook (VMS-enabled)", line = list(color = "#f0a202", width = 3)) |>
        plotly::add_lines(y = ~vms, name = "VMS-allocated", line = list(color = "#12a4b4", width = 3, dash = "dot"))
      p_layout(p, ylab = "t") |> plotly::layout(hovermode = "x unified", xaxis = YR_AXIS, legend = list(y = -0.15))
    })
    output$rect_info <- renderUI({
      if (is.null(sel_rect())) return(div(class = "empty-state", bsicons::bs_icon("cursor"), br(),
        "Red = VMS-allocated landings exceed the VMS-enabled logbook landings; blue = landings missing from the VMS table (unlinked trips, positions outside the rectangle)."))
      d <- rect_data(); ff <- f()
      d <- d[d$yr >= ff$years[1] & d$yr <= ff$years[2], ]
      cov <- 100 * sum(d$vms, na.rm = TRUE) / max(sum(d$le_vms, na.rm = TRUE), 1e-9)
      nov <- 100 * sum(d$days_novms, na.rm = TRUE) / max(sum(d$days, na.rm = TRUE), 1e-9)
      div(class = "kpi-mini",
          div(div(class = "k", "VMS coverage of landings"), div(class = "v", sprintf("%.0f%%", cov))),
          div(div(class = "k", "Days without VMS"), div(class = "v", sprintf("%.0f%%", nov))),
          div(div(class = "k", "Logbook landings"), div(class = "v", paste(fmt_num(sum(d$le_all, na.rm = TRUE), 1), "t"))),
          div(div(class = "k", "VMS fishing hours"), div(class = "v", fmt_num(sum(d$h, na.rm = TRUE), 0))))
    })

    output$vlen <- plotly::renderPlotly({
      d <- q(sprintf("SELECT v::VARCHAR AS v, vms, sum(days) AS days FROM le_cube %s GROUP BY ALL ORDER BY v", where_sql(f())), "logbook: vlen x VMS")
      req(nrow(d))
      d$VMS <- ifelse(d$vms, "Y · VMS-enabled", "N · no VMS")
      p <- plotly::plot_ly(d, x = ~v, y = ~days, color = ~VMS, colors = c("#e5484d", "#12a4b4"), type = "bar")
      p_layout(p, ylab = "fishing days") |> plotly::layout(barmode = "stack")
    })

    output$scatter <- plotly::renderPlotly({
      ff <- f()
      d <- q(sprintf("
        WITH l AS (SELECT rect, sum(kg) FILTER (WHERE vms)/1000 AS le FROM le_cube %s GROUP BY rect),
             v AS (SELECT rect, sum(kg)/1000 AS vms FROM vms_rect %s GROUP BY rect)
        SELECT rect, coalesce(le, 0) AS le, coalesce(vms, 0) AS vms FROM l FULL JOIN v USING (rect)
        WHERE coalesce(le,0) + coalesce(vms,0) > 1", where_sql(ff), where_sql(ff)), "logbook: rect scatter")
      req(nrow(d))
      d$ratio <- pmin(pmax(100 * d$vms / pmax(d$le, 1e-6), 0), 200)
      lim <- range(c(d$le, d$vms)[c(d$le, d$vms) > 0])
      p <- plotly::plot_ly(d, x = ~pmax(le, 0.01), y = ~pmax(vms, 0.01), type = "scatter", mode = "markers", text = ~rect,
                           marker = list(size = 6, color = ~ratio, colorscale = list(c(0, "#2166ac"), c(0.5, "#f7f7f7"), c(1, "#b2182b")),
                                         cmin = 0, cmax = 200, opacity = .8, colorbar = list(title = "VMS %", thickness = 10)),
                           hovertemplate = "%{text}<br>logbook %{x:,.1f} t<br>VMS %{y:,.1f} t<extra></extra>") |>
        plotly::add_lines(x = lim, y = lim, inherit = FALSE, line = list(color = "rgba(127,127,127,.6)", dash = "dash"), showlegend = FALSE)
      p_layout(p, xlab = "logbook landings, VMS-enabled trips (t)", ylab = "VMS-allocated landings (t)", legend = FALSE) |>
        plotly::layout(xaxis = list(type = "log"), yaxis = list(type = "log"))
    })

    output$ctab <- DT::renderDT({
      ff <- f()
      d <- q(sprintf("
        WITH l AS (SELECT c::VARCHAR AS c, sum(days) AS days, 100*sum(days) FILTER (WHERE NOT vms)/sum(days) AS novms,
                          sum(kg) FILTER (WHERE vms) AS le FROM le_cube %s GROUP BY 1),
             v AS (SELECT c::VARCHAR AS c, sum(kg) AS vms FROM vms_rect %s GROUP BY 1)
        SELECT l.c AS Country, round(days) AS \"Fishing days\", round(novms, 1) AS \"%% days no VMS\",
               round(100 * v.vms / l.le, 1) AS \"VMS coverage %%\"
        FROM l LEFT JOIN v USING (c) ORDER BY days DESC", where_sql(ff), where_sql(ff)), "logbook: country table")
      DT::datatable(d, rownames = FALSE, selection = "none", options = list(dom = "t", pageLength = 25)) |>
        DT::formatRound("Fishing days", 0) |>
        DT::formatStyle("% days no VMS", background = DT::styleColorBar(c(0, 100), "rgba(229,72,77,.35)"), backgroundSize = "98% 70%",
                        backgroundRepeat = "no-repeat", backgroundPosition = "center") |>
        DT::formatStyle("VMS coverage %", color = DT::styleInterval(c(85, 101), c("#e5484d", "inherit", "#e5484d")), fontWeight = "bold")
    })
  })
}

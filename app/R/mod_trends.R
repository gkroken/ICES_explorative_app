# -----------------------------------------------------------------------------
# Trends & insights: everything here runs on cube_ts (non-spatial, ~100k rows)
# except the footprint index (cube_1) and the rectangle movers (vms_rect).
# -----------------------------------------------------------------------------

YR_AXIS <- list(tickformat = "d", dtick = 1, title = "")
DIM_LABELS <- c(g = "Gear (L4)", c = "Country", t = "Target (L5)", v = "Vessel length")

trends_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      fill = FALSE, col_widths = c(3, 3, 3, 3),
      uiOutput(ns("vb1")), uiOutput(ns("vb2")), uiOutput(ns("vb3")), uiOutput(ns("vb4"))
    ),
    layout_columns(
      col_widths = c(8, 4), row_heights = "420px",
      card(full_screen = TRUE,
        card_header(class = "d-flex justify-content-between align-items-center",
          span(textOutput(ns("ts_title"), inline = TRUE)),
          div(class = "d-flex gap-2",
            radioButtons(ns("ts_by"), NULL, inline = TRUE, choices = setNames(names(DIM_LABELS), DIM_LABELS), selected = "g"),
            checkboxInput(ns("ts_share"), "100%", FALSE))),
        plotly::plotlyOutput(ns("ts"))),
      card(full_screen = TRUE, card_header(bsicons::bs_icon("lightbulb"), " Auto-insights"), uiOutput(ns("insights")))
    ),
    layout_columns(
      col_widths = c(4, 4, 4), row_heights = "380px",
      card(full_screen = TRUE, card_header("Seasonality · year × month"), plotly::plotlyOutput(ns("heat"))),
      card(full_screen = TRUE,
           card_header(tooltip(span("Spatial footprint ", bsicons::bs_icon("info-circle")),
             "Number of 0.05° c-squares fished, and the number needed to hold 50% / 90% of the selected metric. A shrinking 90% core with a stable total means effort is concentrating.")),
           plotly::plotlyOutput(ns("foot"))),
      card(full_screen = TRUE, card_header("LPUE by gear (kg per fishing hour)"), plotly::plotlyOutput(ns("lpue")))
    ),
    layout_columns(
      col_widths = c(6, 6), row_heights = "400px",
      card(full_screen = TRUE, card_header("Country × gear (selected metric)"), plotly::plotlyOutput(ns("matrix"))),
      card(full_screen = TRUE, card_header("Biggest movers · ICES rectangles (first 3 vs last 3 years)"), DT::DTOutput(ns("movers")))
    )
  )
}

trends_server <- function(id, f, metric) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    mx <- function() mexpr(metric())

    yearly <- reactive({
      q(sprintf("SELECT yr, sum(h) h, sum(kwh) kwh, sum(kg) kg, sum(eur) eur, %s AS v FROM cube_ts %s GROUP BY yr ORDER BY yr",
                mx(), where_sql(f())), "trend: yearly totals")
    })

    vb <- function(title, col, scale, unit, icon, theme) {
      renderUI({
        y <- yearly(); req(nrow(y))
        last <- tail(y, 1); prev <- if (nrow(y) > 1) y[nrow(y) - 1, ] else NULL
        val <- last[[col]] / scale
        chg <- if (!is.null(prev) && prev[[col]] > 0) 100 * (last[[col]] / prev[[col]] - 1) else NA
        spark <- plotly::plot_ly(y, x = ~yr, y = as.formula(paste0("~", col)), type = "scatter", mode = "lines",
                                 line = list(color = "rgba(255,255,255,.85)", width = 2), fill = "tozeroy",
                                 fillcolor = "rgba(255,255,255,.18)", hoverinfo = "none") |>
          plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE), margin = list(t = 0, r = 0, l = 0, b = 0),
                         paper_bgcolor = "transparent", plot_bgcolor = "transparent", showlegend = FALSE) |>
          plotly::config(displayModeBar = FALSE)
        value_box(
          title = sprintf("%s · %d", title, last$yr),
          value = paste(fmt_num(val, 1), unit),
          p(class = "mb-0", style = "font-size:.8rem", if (is.na(chg)) "" else sprintf("%s vs %d", fmt_pct(chg), prev$yr)),
          showcase = spark, showcase_layout = "bottom", theme = theme, full_screen = FALSE, height = "200px"
        )
      })
    }
    output$vb1 <- vb("Fishing hours", "h", 1, "h", "clock", "primary")
    output$vb2 <- vb("kW fishing hours", "kwh", 1, "kWh", "lightning", "teal")
    output$vb3 <- vb("Landings", "kg", 1000, "t", "basket", "indigo")
    output$vb4 <- vb("Landed value", "eur", 1e6, "M€", "currency-euro", "orange")

    output$ts_title <- renderText(sprintf("%s by %s", METRICS[[metric()]]$label, sub("^(\\w)", "\\L\\1", DIM_LABELS[[input$ts_by]], perl = TRUE)))

    output$ts <- plotly::renderPlotly({
      by <- input$ts_by
      d <- q(sprintf("SELECT yr, %s::VARCHAR AS k, %s AS v FROM cube_ts %s GROUP BY ALL ORDER BY yr", by, mx(), where_sql(f())),
             paste("trend by", by))
      req(nrow(d))
      # keep the 9 biggest series, fold the rest into "other"
      tops <- names(sort(tapply(d$v, d$k, sum, na.rm = TRUE), decreasing = TRUE))
      if (length(tops) > 9 && !isTRUE(METRICS[[metric()]]$ratio)) {
        d$k[!d$k %in% tops[1:9]] <- "other"
        d <- aggregate(v ~ yr + k, d, sum)
      }
      ratio <- isTRUE(METRICS[[metric()]]$ratio)
      cols <- pal_for(unique(d$k))
      if (ratio) {
        p <- plotly::plot_ly(d, x = ~yr, y = ~v, color = ~k, colors = cols, type = "scatter", mode = "lines+markers")
      } else {
        p <- plotly::plot_ly(d, x = ~yr, y = ~v, color = ~k, colors = cols, type = "scatter", mode = "none",
                             stackgroup = "one", groupnorm = if (isTRUE(input$ts_share)) "percent" else "",
                             hovertemplate = "%{x} %{fullData.name}: %{y:,.0f}<extra></extra>")
      }
      p_layout(p, ylab = if (isTRUE(input$ts_share)) "% of total" else METRICS[[metric()]]$unit) |>
        plotly::layout(hovermode = "x unified", xaxis = YR_AXIS)
    })

    output$heat <- plotly::renderPlotly({
      d <- q(sprintf("SELECT yr, mo, %s AS v FROM cube_ts %s GROUP BY ALL", mx(), where_sql(f())), "trend: seasonality")
      req(nrow(d))
      yrs <- sort(unique(d$yr))
      z <- matrix(NA_real_, length(yrs), 12)
      z[cbind(match(d$yr, yrs), d$mo)] <- d$v
      # normalise each year so seasonal *shape* is comparable across years
      if (!isTRUE(METRICS[[metric()]]$ratio)) z <- z / rowSums(z, na.rm = TRUE) * 100
      p <- plotly::plot_ly(x = month.abb, y = yrs, z = z, type = "heatmap", colorscale = "Viridis",
                           hovertemplate = "%{y} %{x}: %{z:.1f}<extra></extra>",
                           colorbar = list(title = list(text = if (isTRUE(METRICS[[metric()]]$ratio)) METRICS[[metric()]]$unit else "% of year"), thickness = 10))
      p_layout(p, legend = FALSE) |> plotly::layout(yaxis = list(dtick = 1, tickformat = "d", autorange = "reversed"))
    })

    foot <- reactive({
      m <- if (isTRUE(METRICS[[metric()]]$ratio)) "h" else metric()
      sql <- sprintf("
        WITH cells AS (SELECT yr, x, y, %s AS v FROM cube_1 %s GROUP BY ALL HAVING v > 0),
        r AS (SELECT yr, v, sum(v) OVER (PARTITION BY yr ORDER BY v DESC ROWS UNBOUNDED PRECEDING) / sum(v) OVER (PARTITION BY yr) AS cum FROM cells)
        SELECT yr, count(*) AS total, count(*) FILTER (WHERE cum <= 0.5) AS c50, count(*) FILTER (WHERE cum <= 0.9) AS c90
        FROM r GROUP BY yr ORDER BY yr", mexpr(m), where_sql(f()))
      q(sql, "footprint index (cube_1)")
    })

    output$foot <- plotly::renderPlotly({
      d <- foot(); req(nrow(d))
      p <- plotly::plot_ly(d, x = ~yr) |>
        plotly::add_lines(y = ~total, name = "all fished", line = list(color = "#9aa5b1", width = 2)) |>
        plotly::add_lines(y = ~c90, name = "90% core", line = list(color = "#12a4b4", width = 3)) |>
        plotly::add_lines(y = ~c50, name = "50% core", line = list(color = "#f0a202", width = 3))
      p_layout(p, ylab = "c-squares (0.05°)") |> plotly::layout(hovermode = "x unified", xaxis = YR_AXIS)
    })

    output$lpue <- plotly::renderPlotly({
      d <- q(sprintf("SELECT yr, g::VARCHAR AS g, sum(kg)/nullif(sum(h),0) AS lpue, sum(h) AS h FROM cube_ts %s GROUP BY ALL ORDER BY yr",
                     where_sql(f())), "trend: LPUE")
      req(nrow(d))
      keep <- names(sort(tapply(d$h, d$g, sum), decreasing = TRUE))[1:min(7, length(unique(d$g)))]
      d <- d[d$g %in% keep, ]
      p <- plotly::plot_ly(d, x = ~yr, y = ~lpue, color = ~g, colors = pal_for(unique(d$g)), type = "scatter", mode = "lines+markers")
      p_layout(p, ylab = "kg / h") |> plotly::layout(yaxis = list(type = "log"), xaxis = YR_AXIS)
    })

    output$matrix <- plotly::renderPlotly({
      d <- q(sprintf("SELECT c::VARCHAR AS c, g::VARCHAR AS g, %s AS v FROM cube_ts %s GROUP BY ALL", mx(), where_sql(f())), "trend: country x gear")
      req(nrow(d))
      cs <- names(sort(tapply(d$v, d$c, sum, na.rm = TRUE)))
      gs <- names(sort(tapply(d$v, d$g, sum, na.rm = TRUE), decreasing = TRUE))
      p <- plotly::plot_ly(d, x = ~factor(g, gs), y = ~factor(c, cs), type = "scatter", mode = "markers",
                           marker = list(size = ~sqrt(v / max(v, na.rm = TRUE)) * 38 + 2, color = ~log10(pmax(v, 1e-6)),
                                         colorscale = "Viridis", line = list(width = 0), opacity = .9),
                           text = ~sprintf("%s · %s: %s", c, g, formatC(v, format = "f", digits = 0, big.mark = ",")), hoverinfo = "text")
      p_layout(p, legend = FALSE)
    })

    output$movers <- DT::renderDT({
      ff <- f(); m <- metric()
      y0 <- ff$years[1]; y1 <- ff$years[2]
      req(y1 - y0 >= 3)
      mm <- if (m %in% c("h", "kg", "eur", "lpue", "vpue")) m else "h"  # vms_rect carries hours, kg, value
      fa <- sprintf("yr BETWEEN %d AND %d", y0, y0 + 2); fb <- sprintf("yr BETWEEN %d AND %d", y1 - 2, y1)
      div3 <- if (isTRUE(METRICS[[mm]]$ratio)) "" else "/3"
      sql <- sprintf("
        SELECT rect AS \"Rectangle\", %s%s AS \"First 3y (avg)\", %s%s AS \"Last 3y (avg)\"
        FROM vms_rect %s GROUP BY rect", mexpr(mm, "cube", fa), div3, mexpr(mm, "cube", fb), div3, where_sql(ff))
      d <- tryCatch(q(sql, "movers (vms_rect)"), error = function(e) NULL)
      req(!is.null(d), nrow(d))
      d$Change <- d$`Last 3y (avg)` - d$`First 3y (avg)`
      d$`Change %` <- round(100 * d$Change / pmax(d$`First 3y (avg)`, 1e-9), 0)
      d <- d[order(-abs(d$Change)), ][1:min(60, nrow(d)), ]
      DT::datatable(d, rownames = FALSE, options = list(dom = "tp", pageLength = 9, order = list()), selection = "none") |>
        DT::formatRound(c("First 3y (avg)", "Last 3y (avg)", "Change"), 1) |>
        DT::formatStyle("Change", color = DT::styleInterval(0, c("#e5484d", "#2fb67c")), fontWeight = "bold")
    })

    output$insights <- renderUI({
      ff <- f(); m <- metric(); met <- METRICS[[m]]
      y <- yearly(); req(nrow(y) >= 2)
      out <- list()
      add <- function(icon, html) out[[length(out) + 1]] <<- div(class = "insight", span(class = "ic", bsicons::bs_icon(icon)), div(HTML(html)))
      first <- y[1, ]; last <- y[nrow(y), ]
      ch <- 100 * (last$v / first$v - 1)
      verb <- if (abs(ch) < 1) "was <b>flat</b>" else sprintf("%s <b>%.0f%%</b>", if (ch > 0) "rose" else "fell", abs(ch))
      add(if (ch >= 0) "graph-up-arrow" else "graph-down-arrow",
          sprintf("<b>%s</b> %s between %d and %d (%s → %s %s).", met$label, verb,
                  first$yr, last$yr, fmt_num(first$v, 1), fmt_num(last$v, 1), met$unit))
      if (nrow(y) >= 6 && !isTRUE(met$ratio)) {
        g <- q(sprintf("SELECT g::VARCHAR AS g, %s AS a, %s AS b FROM cube_ts %s GROUP BY g HAVING a > 0",
                       mexpr(m, "cube", sprintf("yr <= %d", first$yr + 2)), mexpr(m, "cube", sprintf("yr >= %d", last$yr - 2)), where_sql(ff)),
               "insight: gear shifts")
        g$r <- 100 * (g$b / g$a - 1)
        g <- g[g$a > 0.02 * sum(g$a), ]
        if (nrow(g) >= 2) {
          up <- g[which.max(g$r), ]; dn <- g[which.min(g$r), ]
          add("arrow-left-right", sprintf("Fastest-growing gear: <b>%s</b> (%s, %s); steepest decline: <b>%s</b> (%s, %s) — last vs first three years.",
                                          up$g, GEAR_NAMES[up$g], fmt_pct(up$r, 0), dn$g, GEAR_NAMES[dn$g], fmt_pct(dn$r, 0)))
        }
      }
      fp <- foot()
      if (nrow(fp) >= 2) {
        a <- fp[1, ]; b <- fp[nrow(fp), ]
        add("bullseye", sprintf("In %d, <b>%s%%</b> of %s came from just <b>%s%%</b> of the fished c-squares. The 90%% core %s from %s to %s c-squares since %d.",
            b$yr, 90, tolower(if (isTRUE(met$ratio)) "fishing hours" else met$label), round(100 * b$c90 / b$total),
            if (b$c90 < a$c90) "contracted" else "expanded", format(a$c90, big.mark = ","), format(b$c90, big.mark = ","), a$yr))
      }
      s <- q(sprintf("SELECT mo, %s AS v FROM cube_ts %s GROUP BY mo ORDER BY v DESC LIMIT 1", mexpr(if (isTRUE(met$ratio)) "h" else m), where_sql(ff)), "insight: peak month")
      if (nrow(s)) add("calendar3", sprintf("Peak month for the selection: <b>%s</b>.", month.name[s$mo]))
      cf <- q(sprintf("SELECT 100.0 * count(*) FILTER (WHERE NoDistinctVessels < 3) / count(*) AS p FROM vms %s",
                      where_sql(ff, "lake")), "insight: confidentiality (lake)")
      add("shield-lock", sprintf("<b>%.0f%%</b> of Table 1 rows in the selection represent fewer than 3 vessels and carry anonymised vessel IDs — relevant for any public product.", cf$p))
      tagList(out)
    })
  })
}

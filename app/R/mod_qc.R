# -----------------------------------------------------------------------------
# QC Lab: automates the ICES VMS/logbook reviewer checklist for one country,
# straight from the Parquet lake, before data are submitted.
# -----------------------------------------------------------------------------

SPEED_RANGE <- list(OTB = c(1, 6), OTM = c(1, 6), PTM = c(1, 6), TBB = c(2, 8), SDN = c(0, 2.5), SSC = c(0, 3),
                    DRB = c(1, 5), PS = c(0, 5), LLS = c(0, 5), GNS = c(0, 3), FPO = c(0, 3))
WIDTH_MAX_KM <- list(OTB = 0.5, TBB = 0.06, DRB = 0.06, SDN = 3, SSC = 3)
VLEN_BOUNDS <- list(VL0006 = c(0, 6), VL0608 = c(6, 8), VL0810 = c(8, 10), VL1012 = c(10, 12), VL1215 = c(12, 15),
                    VL1518 = c(15, 18), VL1824 = c(18, 24), VL2440 = c(24, 40), VL40XX = c(40, 144))
SEV_ORDER <- c(pass = 0, info = 0, minor = 1, major = 2, critical = 3)

qc_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    border = FALSE,
    sidebar = sidebar(
      width = 280, title = "QC run",
      selectInput(ns("country"), "Submitting country", choices = named_choices(DIMS$c, COUNTRY_NAMES), selected = "NO"),
      sliderInput(ns("years"), "Data years", min = DIMS$years[1], max = DIMS$years[2], value = DIMS$years, step = 1, sep = ""),
      p(class = "small text-muted", "Checks mirror the ICES reviewer form: units & scales, fishing speeds by gear, gear widths, vessel lengths, ",
        "ICES area (44°W–30°E, 35–90°N), value submission, discontinuities over time and Table 1 ↔ Table 2 consistency."),
      downloadButton(ns("dl"), "Download findings (CSV)", class = "btn-sm btn-outline-primary w-100"),
      uiOutput(ns("timing"))
    ),
    uiOutput(ns("verdict")),
    layout_columns(
      col_widths = c(5, 7), row_heights = "520px",
      card(full_screen = TRUE, card_header(bsicons::bs_icon("list-check"), " Checklist"), uiOutput(ns("checks"))),
      card(full_screen = TRUE, card_header("Fishing speed by gear · box = p25–p75, whiskers = p1–max, band = plausible range"),
           plotly::plotlyOutput(ns("speed")))
    ),
    layout_columns(
      col_widths = c(6, 6), row_heights = "440px",
      card(full_screen = TRUE, card_header("Unit & scale check · Table 1 (red = outside plausible limits)"), DT::DTOutput(ns("units"))),
      card(full_screen = TRUE, card_header("Consistency over time (red = discontinuity)"), plotly::plotlyOutput(ns("yoy")))
    )
  )
}

qc_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    years_d <- debounce(reactive(input$years), 400)

    base <- reactive({
      sprintf("WHERE CountryCode = %s AND Year BETWEEN %d AND %d", sq(input$country), years_d()[1], years_d()[2])
    })

    res <- reactive({
      w <- base()
      t0 <- proc.time()[[3]]
      speed <- q(sprintf("
        SELECT MetierL4 AS g, count(*) AS n,
               approx_quantile(AverageFishingSpeed, [0.01, 0.25, 0.5, 0.75, 0.95]) AS qs, max(AverageFishingSpeed) AS mx,
               count(*) FILTER (WHERE AverageFishingSpeed > 9) AS n_fast,
               approx_quantile(AverageGearWidth, 0.5) AS w_med, max(AverageGearWidth) AS w_max
        FROM vms %s GROUP BY 1 ORDER BY n DESC", w), "QC: speeds & widths")
      # unit & scale table: one pass, 5 aggregates per column (no UNPIVOT materialisation)
      ucols <- c("FishingHour", "AverageFishingSpeed", "AverageInterval", "AverageVesselLength", "AveragekW", "kWFishingHour",
                 "SweptArea", "TotWeight", "TotValue", "AverageGearWidth", "NumberOfRecords", "NoDistinctVessels")
      aggs <- unlist(lapply(seq_along(ucols), function(i) sprintf(
        "min(%1$s)::DOUBLE AS a%2$d_min, approx_quantile(%1$s, 0.01)::DOUBLE AS a%2$d_p01, approx_quantile(%1$s, 0.5)::DOUBLE AS a%2$d_median, approx_quantile(%1$s, 0.99)::DOUBLE AS a%2$d_p99, max(%1$s)::DOUBLE AS a%2$d_max",
        ucols[i], i)))
      u1 <- q(sprintf("SELECT %s FROM vms %s", paste(aggs, collapse = ", "), w), "QC: unit table (1 pass)")
      unit_tab <- data.frame(param = ucols, t(vapply(seq_along(ucols), function(i)
        unlist(u1[1, sprintf("a%d_%s", i, c("min", "p01", "median", "p99", "max"))]), numeric(5))))
      names(unit_tab) <- c("param", "min", "p01", "median", "p99", "max")
      yearly <- q(sprintf("
        SELECT Year, sum(FishingHour) AS FishingHour, sum(kWFishingHour) AS kWFishingHour, sum(TotWeight) AS TotWeight,
               sum(TotValue) AS TotValue, sum(NumberOfRecords) AS Records,
               sum(kWFishingHour) / sum(FishingHour) AS kw_implied, sum(AveragekW * FishingHour) / sum(FishingHour) AS kw_mean,
               100.0 * count(*) FILTER (WHERE TotValue IS NULL) / count(*) AS pct_value_na,
               median(AverageInterval) AS int_med, count(*) FILTER (WHERE AverageInterval > 4) AS n_int_bad,
               count(*) FILTER (WHERE lon < -44 OR lon > 30 OR lat < 35 OR lat > 90) AS n_out,
               count(*) FILTER (WHERE AverageVesselLength < 6 OR AverageVesselLength > 144) AS n_len_bad,
               count(*) FILTER (WHERE FishingHour <= 0) AS n_zero, count(*) AS n,
               count(*) FILTER (WHERE NoDistinctVessels < 3) AS n_conf
        FROM vms %s GROUP BY Year ORDER BY Year", w), "QC: yearly (lake)")
      lenclass <- q(sprintf("SELECT VesselLengthRange AS v, min(AverageVesselLength) AS lo, max(AverageVesselLength) AS hi, count(*) AS n,
                              %s AS n_out FROM vms %s GROUP BY 1 ORDER BY 1",
                            paste0("count(*) FILTER (WHERE ", paste(sprintf("(VesselLengthRange = '%s' AND (AverageVesselLength < %g OR AverageVesselLength > %g))",
                                   names(VLEN_BOUNDS), sapply(VLEN_BOUNDS, `[`, 1) - 0.5, sapply(VLEN_BOUNDS, `[`, 2) + 0.5), collapse = " OR "), ")"), w),
                    "QC: length classes")
      le <- q(sprintf("SELECT Year, sum(TotWeight) FILTER (WHERE VMSEnabled = 'Y') AS le_vms, sum(TotWeight) AS le_all,
                              count(*) FILTER (WHERE FishingDays <= 0) AS n_zero_days,
                              count(*) FILTER (WHERE NOT regexp_matches(ICESrectangle, '^[0-9]{2}[A-M][0-9]$')) AS n_badrect
                       FROM logbook %s GROUP BY Year ORDER BY Year", w), "QC: logbook (lake)")
      list(speed = speed, unit_tab = unit_tab, yearly = yearly, lenclass = lenclass, le = le,
           ms = (proc.time()[[3]] - t0) * 1000)
    })

    checks <- reactive({
      r <- res(); y <- r$yearly; sp <- r$speed
      out <- list()
      add <- function(section, title, sev, detail, fix = "") out[[length(out) + 1]] <<- list(section = section, title = title, severity = sev, detail = detail, fix = fix)
      req(nrow(y))

      # 1 ICES area
      n_out <- sum(y$n_out)
      add("VMS use of space", "C-squares inside ICES convention area", if (n_out > 0) "major" else "pass",
          if (n_out > 0) sprintf("%s rows outside 44°W–30°E / 35–90°N (years: %s).", format(n_out, big.mark = ","), paste(y$Year[y$n_out > 0], collapse = ", "))
          else "All c-squares within the ICES area.",
          "Remove or correct positions outside the ICES area; check sign of longitudes.")

      # 2 speeds
      sp$q01 <- vapply(sp$qs, `[`, 0, 1); sp$q50 <- vapply(sp$qs, `[`, 0, 3); sp$q95 <- vapply(sp$qs, `[`, 0, 5)
      bad <- c(); seine <- c()
      for (i in seq_len(nrow(sp))) {
        rg <- SPEED_RANGE[[sp$g[i]]]; if (is.null(rg)) next
        if (sp$mx[i] > max(9, rg[2] * 1.6) || sp$q95[i] > rg[2] * 1.3) bad <- c(bad, sprintf("%s (max %.1f kn, %s rows > 9 kn)", sp$g[i], sp$mx[i], format(sp$n_fast[i], big.mark = ",")))
        if (sp$g[i] %in% c("SDN", "SSC") && sp$q50[i] > 2) seine <- c(seine, sprintf("%s median %.1f kn", sp$g[i], sp$q50[i]))
      }
      add("VMS data visualisations", "Fishing speeds realistic for gear type", if (length(bad)) "major" else "pass",
          if (length(bad)) paste("Speeds outside realistic ranges:", paste(bad, collapse = "; ")) else "All gear speed distributions within plausible ranges.",
          "Apply a cap to fishing speeds at realistic maxima per gear, or check units / activity classification.")
      add("VMS data visualisations", "Seine speeds centred near zero (SDN/SSC)", if (length(seine)) "minor" else "pass",
          if (length(seine)) paste("Demersal seine speeds look like towing:", paste(seine, collapse = "; ")) else "Seine speeds centred near zero.",
          "Check activity determination of demersal seines.")

      # 3 gear width units
      wb <- c()
      for (i in seq_len(nrow(sp))) {
        mx <- WIDTH_MAX_KM[[sp$g[i]]]
        if (!is.null(mx) && !is.na(sp$w_med[i]) && sp$w_med[i] > mx) wb <- c(wb, sprintf("%s median %.3g", sp$g[i], sp$w_med[i]))
      }
      add("Unit and scale checks", "Gear width reported in km", if (length(wb)) "critical" else "pass",
          if (length(wb)) paste("Gear widths look like metres:", paste(wb, collapse = "; ")) else "Gear widths consistent with km.",
          "Convert AverageGearWidth to km (divide by 1000) and re-derive swept area.")

      # 4 vessel lengths
      nl <- sum(y$n_len_bad); lc <- r$lenclass
      add("Unit and scale checks", "Vessel lengths plausible (6–144 m)", if (nl > 0) "major" else "pass",
          if (nl > 0) sprintf("%s rows with AverageVesselLength < 6 m or > 144 m (years: %s).", format(nl, big.mark = ","), paste(y$Year[y$n_len_bad > 0], collapse = ", ")) else "All average vessel lengths within 6–144 m.",
          "Check length units (feet vs metres) for affected records.")
      nlc <- sum(lc$n_out)
      add("Unit and scale checks", "Average length consistent with VesselLengthRange", if (nlc > 0) "minor" else "pass",
          if (nlc > 0) sprintf("%s rows where AverageVesselLength falls outside its DATSU length class (%s).", format(nlc, big.mark = ","),
                               paste(lc$v[lc$n_out > 0], collapse = ", ")) else "Length classes agree with reported lengths.",
          "Recompute LENGTHCAT from VE_LEN with the DATSU breaks.")

      # 5 intervals
      ni <- sum(y$n_int_bad)
      add("Unit and scale checks", "VMS intervals from minutes to a few hours", if (ni > 0) "minor" else "pass",
          if (ni > 0) sprintf("%s rows with AverageInterval > 4 h — likely minutes (years: %s).", format(ni, big.mark = ","), paste(y$Year[y$n_int_bad > 0], collapse = ", "))
          else sprintf("Median interval %.2f h.", stats::median(y$int_med)),
          "Report INTV in hours.")

      # 6 value submitted
      nv <- y$Year[y$pct_value_na > 50]
      add("Logbook / value", "Value submitted, consistent over time", if (length(nv)) "major" else "pass",
          if (length(nv)) sprintf("TotValue missing for most records in %s.", paste(nv, collapse = ", ")) else "Value present for all years.",
          "Submit TotValue (EUR) for the missing years or document why it is unavailable.")

      # 7 discontinuities
      disc <- function(x) {
        if (length(x) < 4) return(rep(FALSE, length(x)))
        vapply(seq_along(x), function(i) {
          nb <- x[setdiff(max(1, i - 2):min(length(x), i + 2), i)]
          m <- stats::median(nb, na.rm = TRUE)
          is.finite(m) && m > 0 && is.finite(x[i]) && abs(log(x[i] / m)) > log(2)
        }, logical(1))
      }
      dd <- c()
      for (col in c("FishingHour", "kWFishingHour", "TotWeight", "TotValue", "Records")) {
        fl <- disc(y[[col]]); if (any(fl)) dd <- c(dd, sprintf("%s in %s", col, paste(y$Year[fl], collapse = ", ")))
      }
      add("Summary tables", "No discontinuities in hours, kW, kWh, landings, value", if (length(dd)) "major" else "pass",
          if (length(dd)) paste("Year-on-year jumps > 2× vs neighbouring years:", paste(dd, collapse = "; ")) else "Annual totals change smoothly.",
          "Investigate the flagged years for unit changes or processing errors.")

      # 8 kW consistency
      ratio <- y$kw_implied / y$kw_mean
      kb <- y$Year[is.finite(ratio) & (ratio > 1.25 | ratio < 0.8)]
      add("Unit and scale checks", "kWFishingHour = kW × fishing hours", if (length(kb)) "major" else "pass",
          if (length(kb)) sprintf("Implied kW (kWh ÷ hours) differs from AveragekW by > 25%% in %s (ratio up to %.1f×).", paste(kb, collapse = ", "), max(ratio, na.rm = TRUE))
          else "kW-hours consistent with engine power and hours.",
          "Recompute kWFishingHour as VE_KW × INTV.")

      # 9 Table 1 vs Table 2
      le <- merge(y[, c("Year", "TotWeight")], r$le, by = "Year")
      cov <- le$TotWeight / le$le_vms
      cb <- le$Year[is.finite(cov) & (cov < 0.8 | cov > 1.05)]
      add("Logbook vs VMS", "VMS landings align with VMS-enabled logbook landings", if (length(cb)) "minor" else "pass",
          sprintf("VMS ÷ logbook (VMS-enabled) landings: %.0f–%.0f%% across years%s.", 100 * min(cov, na.rm = TRUE), 100 * max(cov, na.rm = TRUE),
                  if (length(cb)) paste0("; outside 80–105% in ", paste(cb, collapse = ", ")) else ""),
          "Check trip linkage between ERS and VMS (unlinked trips).")

      # 10 zero effort + DATSU rectangles
      nz <- sum(y$n_zero) + sum(r$le$n_zero_days)
      add("Summary tables", "Fishing hours / days > 0", if (nz > 0) "minor" else "pass",
          if (nz > 0) sprintf("%s rows with zero effort.", nz) else "All Table 1 hours and Table 2 days are positive.", "Drop zero-effort records.")
      nr <- sum(r$le$n_badrect)
      add("DATSU vocabulary", "ICES rectangles valid", if (nr > 0) "major" else "pass",
          if (nr > 0) sprintf("%s logbook rows with malformed rectangle codes.", nr) else "All rectangle codes well-formed.", "Fix StatRec codes.")
      add("Confidentiality", "Rows with fewer than 3 vessels", "info",
          sprintf("%.1f%% of Table 1 rows carry AnonymizedVesselID (NoDistinctVessels < 3).", 100 * sum(y$n_conf) / sum(y$n)), "")
      out
    })

    output$verdict <- renderUI({
      ch <- checks()
      sev <- vapply(ch, `[[`, "", "severity")
      nc <- sum(sev == "critical"); nm <- sum(sev == "major"); nn <- sum(sev == "minor")
      cls <- if (nc) "bad" else if (nm + nn) "warn" else "ok"
      verdict <- if (nc) "Rejected — critical issues" else if (nm + nn) "Approved with conditions" else "Approved"
      div(class = paste("qc-verdict mb-3", cls),
          bsicons::bs_icon(if (nc) "x-octagon" else if (nm + nn) "exclamation-triangle" else "check2-circle", size = "2rem"),
          div(div(class = "big", sprintf("%s · %s %d–%d", verdict, COUNTRY_NAMES[input$country] %||% input$country, years_d()[1], years_d()[2])),
              div(sprintf("%d critical · %d major · %d minor · %d passed", nc, nm, nn, sum(sev == "pass")))))
    })

    output$checks <- renderUI({
      ch <- checks()
      ch <- ch[order(-SEV_ORDER[vapply(ch, `[[`, "", "severity")])]
      tagList(lapply(ch, function(x) {
        s <- x$severity
        div(class = "qc-check",
            div(class = paste("qc-dot", if (s == "info") "minor" else s), switch(s, pass = "✓", info = "i", "!")),
            div(div(class = "qc-title", x$title, span(class = paste("qc-sev", s), s)),
                div(class = "qc-detail", tags$i(x$section), " · ", x$detail),
                if (nzchar(x$fix) && s %in% c("minor", "major", "critical")) div(class = "qc-detail", tags$b("Fix: "), x$fix)))
      }))
    })

    output$timing <- renderUI({
      r <- res()
      div(class = "small text-muted mt-3 mono", sprintf("7 lake queries · %.0f ms", r$ms))
    })

    output$speed <- plotly::renderPlotly({
      sp <- res()$speed; req(nrow(sp))
      qs <- do.call(rbind, sp$qs)
      gs <- sp$g
      p <- plotly::plot_ly(type = "box", x = gs, q1 = qs[, 2], median = qs[, 3], q3 = qs[, 4],
                           lowerfence = qs[, 1], upperfence = sp$mx, name = "speed",
                           marker = list(color = "#12a4b4"), line = list(color = "#12a4b4"), fillcolor = "rgba(18,164,180,.35)")
      shapes <- lapply(seq_along(gs), function(i) {
        rg <- SPEED_RANGE[[gs[i]]]; if (is.null(rg)) return(NULL)
        list(type = "rect", xref = "x", yref = "y", x0 = i - 1 - 0.45, x1 = i - 1 + 0.45, y0 = rg[1], y1 = rg[2],
             fillcolor = "rgba(47,182,124,.13)", line = list(width = 0), layer = "below")
      })
      p_layout(p, ylab = "knots", legend = FALSE) |>
        plotly::layout(shapes = Filter(Negate(is.null), shapes), xaxis = list(categoryorder = "array", categoryarray = gs))
    })

    output$units <- DT::renderDT({
      u <- res()$unit_tab
      limits <- list(AverageFishingSpeed = c(0, 9), AverageInterval = c(0.01, 4), AverageVesselLength = c(6, 144),
                     AverageGearWidth = c(0, 3), AveragekW = c(5, 9000), NoDistinctVessels = c(1, 500))
      ord <- c("FishingHour", "AverageFishingSpeed", "AverageInterval", "AverageVesselLength", "AveragekW", "kWFishingHour",
               "SweptArea", "TotWeight", "TotValue", "AverageGearWidth", "NumberOfRecords", "NoDistinctVessels")
      u <- u[match(ord, u$param), ]
      cells <- c("min", "p01", "median", "p99", "max")
      show <- u[, c("param", cells)]
      for (cn in cells) {
        show[[cn]] <- mapply(function(p, v) {
          lim <- limits[[p]]
          txt <- if (is.na(v)) "NA" else formatC(v, digits = 3, format = "g", big.mark = ",")
          if (!is.null(lim) && !is.na(v) && (v < lim[1] || v > lim[2])) sprintf("<span style='background:rgba(229,72,77,.85);color:#fff;padding:1px 6px;border-radius:4px'>%s</span>", txt) else txt
        }, u$param, u[[cn]])
      }
      names(show)[1] <- "Parameter"
      DT::datatable(show, escape = FALSE, rownames = FALSE, selection = "none", options = list(dom = "t", pageLength = 20, ordering = FALSE, scrollX = TRUE))
    })

    output$yoy <- plotly::renderPlotly({
      y <- res()$yearly; req(nrow(y))
      cols <- c("FishingHour", "kWFishingHour", "TotWeight", "TotValue", "Records")
      plots <- lapply(cols, function(col) {
        x <- y[[col]]
        fl <- vapply(seq_along(x), function(i) {
          nb <- x[setdiff(max(1, i - 2):min(length(x), i + 2), i)]
          m <- stats::median(nb, na.rm = TRUE); is.finite(m) && m > 0 && is.finite(x[i]) && abs(log(x[i] / m)) > log(2)
        }, logical(1))
        plotly::plot_ly(x = y$Year, y = x, type = "scatter", mode = "lines+markers", name = col,
                        line = list(color = "#12a4b4"), marker = list(color = ifelse(fl, "#e5484d", "#12a4b4"), size = ifelse(fl, 11, 6)),
                        hovertemplate = paste0(col, " %{x}: %{y:,.0f}<extra></extra>")) |>
          plotly::layout(annotations = list(list(text = col, x = 0.02, y = 1, xref = "paper", yref = "paper", showarrow = FALSE, xanchor = "left", font = list(size = 10))))
      })
      p_layout(plotly::subplot(plots, nrows = length(cols), shareX = TRUE, titleY = FALSE), legend = FALSE) |>
        plotly::layout(margin = list(t = 10, l = 60))
    })

    output$dl <- downloadHandler(
      filename = function() sprintf("QC_findings_%s_%d-%d.csv", input$country, years_d()[1], years_d()[2]),
      content = function(file) {
        ch <- checks()
        d <- data.frame(
          Country = input$country,
          Section = vapply(ch, `[[`, "", "section"), Check = vapply(ch, `[[`, "", "title"),
          Severity = vapply(ch, `[[`, "", "severity"), Issue = vapply(ch, `[[`, "", "detail"),
          RequiredFix = vapply(ch, `[[`, "", "fix"))
        utils::write.csv(d[order(-SEV_ORDER[d$Severity]), ], file, row.names = FALSE)
      }
    )
  })
}

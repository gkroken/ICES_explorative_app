# Plot helpers: one consistent, theme-neutral plotly look (works in light + dark).

PAL <- c("#12a4b4", "#f0a202", "#7b61ff", "#e5484d", "#2fb67c", "#0b6e99", "#ff7eb6",
         "#8a6d3b", "#9aa5b1", "#00c2ff", "#c2d94c", "#ff9f40", "#5b8def", "#b07aa1")
GEAR_COL <- setNames(PAL[seq_along(names(GEAR_NAMES))], names(GEAR_NAMES))
pal_for <- function(keys) {
  keys <- as.character(keys)
  out <- GEAR_COL[keys]
  miss <- is.na(out)
  out[miss] <- rep(PAL, length.out = sum(miss))
  setNames(unname(out), keys)
}

p_layout <- function(p, xlab = NULL, ylab = NULL, legend = TRUE, ...) {
  grid <- "rgba(127,127,127,0.18)"
  plotly::layout(p,
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    font = list(family = "Inter, system-ui, sans-serif", size = 11, color = "#8a99a6"),
    xaxis = list(title = xlab, gridcolor = grid, zerolinecolor = grid, linecolor = grid),
    yaxis = list(title = ylab, gridcolor = grid, zerolinecolor = grid, linecolor = grid, separatethousands = TRUE),
    margin = list(l = 50, r = 12, t = 10, b = 40),
    legend = list(orientation = "h", y = -0.18, font = list(size = 10)),
    showlegend = legend, hoverlabel = list(font = list(family = "JetBrains Mono, monospace", size = 11)),
    ...
  ) |> plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

fmt_num <- function(x, d = 0) {
  if (!length(x) || is.na(x)) return("–")
  a <- abs(x)
  if (a >= 1e9) sprintf("%.2f B", x / 1e9)
  else if (a >= 1e6) sprintf("%.2f M", x / 1e6)
  else if (a >= 1e4) sprintf("%.1f k", x / 1e3)
  else formatC(x, format = "f", digits = d, big.mark = ",")
}
fmt_pct <- function(x, d = 1) if (!length(x) || !is.finite(x)) "–" else sprintf("%s%.*f%%", ifelse(x > 0, "+", ""), d, x)

bar_rows <- function(labels, values, unit = "", colours = NULL, wide = FALSE) {
  if (!length(values)) return(htmltools::div(class = "empty-state", "No data"))
  mx <- max(values, na.rm = TRUE)
  htmltools::tagList(lapply(seq_along(values), function(i) {
    htmltools::div(class = if (wide) "bar-row wide" else "bar-row",
      htmltools::span(class = "lbl", title = labels[i], labels[i]),
      htmltools::div(htmltools::div(class = "bar", style = sprintf("width:%.1f%%;%s", 100 * values[i] / mx,
        if (!is.null(colours)) sprintf("background:%s", colours[i]) else ""))),
      htmltools::span(class = "num", fmt_num(values[i], 1)))
  }))
}

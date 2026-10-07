# =============================================================================
#  ICES VMS & Logbook Explorer
#  bslib + leaflet front-end over a DuckDB / Parquet store of simulated
#  ICES-wide data call submissions (Table 1 = VMS, Table 2 = logbook).
# =============================================================================
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
})
options(shiny.autoload.r = TRUE)  # sources R/*.R (db.R first, alphabetically)

theme <- bs_theme(
  version = 5, preset = "shiny",
  primary = "#0b7f95", secondary = "#5d6b78", success = "#2fb67c", warning = "#f0a202", danger = "#e5484d",
  info = "#12a4b4",
  base_font = font_collection(font_google("Inter", local = FALSE), "system-ui", "-apple-system", "Segoe UI", "sans-serif"),
  heading_font = font_collection(font_google("Inter", local = FALSE), "system-ui", "sans-serif"),
  code_font = font_collection(font_google("JetBrains Mono", local = FALSE), "ui-monospace", "SFMono-Regular", "Menlo", "monospace"),
  "navbar-bg" = "#0b1d2a",
  "border-radius" = "0.6rem", "card-border-radius" = "0.8rem"
)

filters_sidebar <- sidebar(
  id = "global_sb", width = 300, open = "desktop",
  div(class = "d-flex justify-content-between align-items-center",
      span(class = "eyebrow", "Filters · all tabs"),
      actionLink("reset", "reset", class = "filter-reset")),
  sliderInput("years", "Years", min = DIMS$years[1], max = DIMS$years[2], value = DIMS$years, step = 1, sep = ""),
  selectizeInput("months", "Months", choices = setNames(1:12, month.abb), multiple = TRUE,
                 options = list(placeholder = "All months", plugins = list("remove_button"))),
  selectizeInput("c", "Country", choices = named_choices(DIMS$c, COUNTRY_NAMES), multiple = TRUE,
                 options = list(placeholder = "All countries", plugins = list("remove_button"))),
  selectizeInput("g", "Gear · Métier L4", choices = named_choices(DIMS$g, GEAR_NAMES), multiple = TRUE,
                 options = list(placeholder = "All gears", plugins = list("remove_button"))),
  selectizeInput("t", "Target assemblage · L5", choices = DIMS$t, multiple = TRUE,
                 options = list(placeholder = "All targets", plugins = list("remove_button"))),
  selectizeInput("v", "Vessel length", choices = DIMS$v, multiple = TRUE,
                 options = list(placeholder = "All lengths", plugins = list("remove_button"))),
  hr(class = "my-1"),
  radioButtons("metric", "Metric", choices = metric_choices, selected = "h"),
  hr(class = "my-1"),
  div(class = "eyebrow", "Compare periods (map)"),
  input_switch("cmp_on", "Show change vs. another period", FALSE),
  conditionalPanel("input.cmp_on",
    sliderInput("cmp_years", "Baseline period", min = DIMS$years[1], max = DIMS$years[2],
                value = c(DIMS$years[1], DIMS$years[1] + 3), step = 1, sep = ""),
    radioButtons("cmp_type", NULL, inline = TRUE, choices = c("Absolute" = "abs", "% change" = "pct")),
    p(class = "small text-muted", "Map shows selected years minus baseline, as per-year means. Red = more, blue = less.")
  )
)

ui <- page_navbar(
  title = tags$span(tags$span(class = "brand-mark", "⚓"), "VMS · Logbook Explorer",
                    tags$span(class = "brand-sub", "ICES data call · simulated")),
  id = "nav", theme = theme, fillable = "Map", sidebar = filters_sidebar,
  header = tags$head(
    tags$link(rel = "stylesheet", href = "app.css"),
    tags$script(src = "csquare-layer.js")
  ),
  nav_panel("Map", icon = bsicons::bs_icon("map"), map_ui("explore")),
  nav_panel("Trends", icon = bsicons::bs_icon("graph-up"), trends_ui("trends")),
  nav_panel("VMS ↔ Logbook", icon = bsicons::bs_icon("arrow-left-right"), logbook_ui("le")),
  nav_panel("QC Lab", icon = bsicons::bs_icon("clipboard2-check"), qc_ui("qc")),
  nav_panel("Under the hood", icon = bsicons::bs_icon("cpu"), about_ui("about")),
  nav_spacer(),
  nav_item(tags$span(id = "perf-hud", "⚡ ready")),
  nav_item(input_dark_mode(id = "dark_mode", mode = "dark"))
)

server <- function(input, output, session) {
  # global filter state, debounced so that multi-select typing doesn't fire queries
  f <- debounce(reactive(list(
    years = as.integer(input$years), months = input$months, c = input$c, g = input$g, t = input$t, v = input$v
  )), 300)
  metric <- reactive(input$metric)
  cmp <- reactive(list(on = isTRUE(input$cmp_on), years = as.integer(input$cmp_years), type = input$cmp_type))
  dark <- reactive(input$dark_mode %||% "dark")

  observeEvent(input$reset, {
    updateSliderInput(session, "years", value = DIMS$years)
    for (id in c("months", "c", "g", "t", "v")) updateSelectizeInput(session, id, selected = character(0))
    updateRadioButtons(session, "metric", selected = "h")
    update_switch("cmp_on", value = FALSE)
  })

  # switching compare on: if the selection overlaps the baseline, move it to the latest years
  observeEvent(input$cmp_on, {
    if (isTRUE(input$cmp_on) && input$years[1] <= input$cmp_years[2])
      updateSliderInput(session, "years", value = c(DIMS$years[2] - 3, DIMS$years[2]))
  })

  # the QC Lab has its own controls; tuck the global filters away there
  observeEvent(input$nav, {
    tog <- if (exists("toggle_sidebar", asNamespace("bslib"))) bslib::toggle_sidebar else bslib::sidebar_toggle
    tog("global_sb", open = !identical(input$nav, "QC Lab"))
  }, ignoreInit = TRUE)

  map_server("explore", f, metric, cmp, dark)
  trends_server("trends", f, metric)
  logbook_server("le", f, dark)
  qc_server("qc")
  about_server("about")
}

shinyApp(ui, server)

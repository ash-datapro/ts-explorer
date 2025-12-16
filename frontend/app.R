# app.R — Time-Series Explorer & Forecast Monitor (module-driven)
# UI fixes: no overlap, larger KPI tiles, wider sidebar, improved gray sidebar styling

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(DT)
  library(plotly)
  library(scales)
  library(httr)
  library(jsonlite)
})

# ---------------------------
# Small helpers
# ---------------------------
`%||%` = function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

safe_source = function(path) {
  if (file.exists(path)) {
    source(path, local = TRUE)
    TRUE
  } else {
    FALSE
  }
}

# ---------------------------
# Source helpers (modules)
# ---------------------------
safe_source("R/utils.R")
safe_source("R/sidebar.R")
safe_source("R/metrics.R")
safe_source("R/plots.R")
safe_source("R/prediction.R")
safe_source("R/insights.R")
safe_source("R/about.R")

has_sidebar = exists("sidebar_ui", mode = "function") && exists("sidebar_server", mode = "function")
has_metrics = exists("metrics_ui", mode = "function") && exists("metrics_server", mode = "function")
has_pred = exists("prediction_ui", mode = "function") && exists("prediction_server", mode = "function")
has_about = exists("about_ui", mode = "function") && exists("about_server", mode = "function")
has_insights = exists("insights_ui", mode = "function") && exists("insights_server", mode = "function")

# ---------------------------
# Fallback helpers (only used if modules not available)
# ---------------------------
value_box = function(title, value, subtitle = NULL) {
  div(
    class = "value-box",
    div(class = "vb-title", title),
    div(class = "vb-value", value),
    if (!is.null(subtitle)) div(class = "vb-sub", subtitle)
  )
}

compute_features_local = function(df) {
  df |>
    arrange(date) |>
    mutate(
      amzn = as.numeric(amzn),
      log_price = log(amzn),
      ret_1 = log_price - lag(log_price),
      roll_sd_20 = slider::slide_dbl(ret_1, sd, .before = 19, .complete = TRUE, na.rm = TRUE),
      roll_sd_60 = slider::slide_dbl(ret_1, sd, .before = 59, .complete = TRUE, na.rm = TRUE),
      peak = cummax(amzn),
      drawdown = amzn / peak - 1
    )
}

compute_features_fn = function(df) {
  if (exists("compute_features", mode = "function")) return(compute_features(df))
  compute_features_local(df)
}

plotly_empty_msg = function(msg = "No data in selected range.") {
  plotly::plotly_empty(type = "scatter", mode = "lines") |>
    layout(
      template = "plotly_white",
      annotations = list(list(
        text = msg, x = 0.5, y = 0.5, xref = "paper", yref = "paper",
        showarrow = FALSE, font = list(size = 14)
      )),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    )
}

plot_price_inline = function(df, log_scale = FALSE) {
  y = if (log_scale) df$log_price else df$amzn
  ylab = if (log_scale) "log(price)" else "price"
  
  plot_ly(df, x = ~date, y = y, type = "scatter", mode = "lines",
          hovertemplate = paste0("Date: %{x}<br>", ylab, ": %{y}<extra></extra>")
  ) |>
    layout(template = "plotly_white", title = list(text = "AMZN price"),
           xaxis = list(title = ""), yaxis = list(title = ylab)
    )
}

plot_returns_inline = function(df) {
  d = df |> filter(!is.na(ret_1))
  plot_ly(d, x = ~date, y = ~ret_1, type = "scatter", mode = "lines",
          hovertemplate = "Date: %{x}<br>ret: %{y:.6f}<extra></extra>"
  ) |>
    layout(template = "plotly_white", title = list(text = "Daily log returns"),
           xaxis = list(title = ""), yaxis = list(title = "log return")
    )
}

plot_roll_vol_inline = function(df, window = 60) {
  col = if (window == 20) "roll_sd_20" else "roll_sd_60"
  d = df |> filter(!is.na(.data[[col]]))
  plot_ly(d, x = ~date, y = d[[col]], type = "scatter", mode = "lines",
          hovertemplate = paste0("Date: %{x}<br>sd(", window, "): %{y:.6f}<extra></extra>")
  ) |>
    layout(
      template = "plotly_white",
      title = list(text = paste0("Rolling volatility (", window, "d SD of returns)")),
      xaxis = list(title = ""),
      yaxis = list(title = "sd(log return)")
    )
}

plot_drawdown_inline = function(df) {
  plot_ly(df, x = ~date, y = ~drawdown, type = "scatter", mode = "lines",
          hovertemplate = "Date: %{x}<br>dd: %{y:.2%}<extra></extra>"
  ) |>
    layout(template = "plotly_white", title = list(text = "Drawdown"),
           xaxis = list(title = ""), yaxis = list(title = "drawdown", tickformat = ".0%")
    )
}

plot_price_fn = function(df, log_scale = FALSE) {
  if (exists("plot_price_ts", mode = "function")) return(plot_price_ts(df, log_scale = log_scale))
  plot_price_inline(df, log_scale = log_scale)
}
plot_returns_fn = function(df) {
  if (exists("plot_returns_ts", mode = "function")) return(plot_returns_ts(df))
  plot_returns_inline(df)
}
plot_roll_vol_fn = function(df, window = 60) {
  if (exists("plot_roll_vol_ts", mode = "function")) return(plot_roll_vol_ts(df, window = window))
  plot_roll_vol_inline(df, window = window)
}
plot_drawdown_fn = function(df) {
  if (exists("plot_drawdown_ts", mode = "function")) return(plot_drawdown_ts(df))
  plot_drawdown_inline(df)
}

# ---------------------------
# Diagnostics helpers
# ---------------------------
resolve_reports_dir = function() {
  candidates = c("reports", "backend/reports", "../backend/reports", "../../backend/reports")
  for (p in candidates) if (dir.exists(p)) return(normalizePath(p, winslash = "/", mustWork = FALSE))
  NA_character_
}

# ---------------------------
# UI
# ---------------------------
ui = bslib::page_sidebar(
  title = "Time-Series Explorer",
  fillable = FALSE,
  theme = bs_theme(
    version = 5,
    bootswatch = "zephyr",
    primary = "#2563eb",
    bg = "#f6f8fc",
    fg = "#0f172a",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter")
  ),
  
  header = tagList(
    div(class = "app-subtitle",
        "Leakage-safe time-series EDA, backtests, and API-based forecasting."
    )
  ),
  
  sidebar = bslib::sidebar(
    width = 400,
    open = "desktop",
    class = "app-sidebar",
    if (has_sidebar) {
      sidebar_ui("sidebar")
    } else {
      tagList(
        h4("Controls"),
        p(class = "text-muted", "Filter the visible window and configure forecast inputs."),
        
        h6("Data window", class = "section-h"),
        dateRangeInput(
          "date_range", "Date range",
          start = Sys.Date() - 365 * 3, end = Sys.Date(),
          format = "yyyy-mm-dd"
        ),
        selectInput(
          "view_mode", "Series view",
          choices = c("Price" = "price", "Log Price" = "log"),
          selected = "price"
        ),
        selectInput(
          "vol_window", "Volatility window",
          choices = c("20 trading days" = "20", "60 trading days" = "60"),
          selected = "60"
        ),
        checkboxInput("show_returns", "Show returns panel", value = TRUE),
        
        hr(),
        actionButton("refresh_btn", "↻ Refresh Data", class = "btn-primary w-100"),
        div(style = "margin-top:10px;",
            tags$small(class = "text-muted", textOutput("last_updated", inline = TRUE))
        ),
        
        hr(),
        h6("Forecast request", class = "section-h"),
        dateInput("as_of", "As-of date", value = Sys.Date(), format = "yyyy-mm-dd"),
        selectInput("horizon", "Horizon (days)", choices = c("1" = 1, "5" = 5, "20" = 20), selected = 5),
        actionButton("run_pred", "Run prediction", class = "btn-outline-primary w-100"),
        div(style = "margin-top:8px;",
            tags$small(class = "text-muted", "Backend API: /predict")
        )
      )
    }
  ),
  
  tags$div(class = "runbar-wrap", uiOutput("runbar")),
  
  bslib::navset_card_underline(
    bslib::nav_panel(
      "Overview",
      
      bslib::card(
        bslib::card_header(
          div(class = "d-flex justify-content-between align-items-center",
              span(class = "text-muted", textOutput("row_count", inline = TRUE)),
              downloadButton("download_filtered", "Download CSV", class = "btn-sm btn-outline-secondary")
          )
        ),
        bslib::card_body(
          div(
            class = "kpi-wrap",
            if (has_metrics) {
              metrics_ui("metrics")
            } else {
              fluidRow(
                column(3, uiOutput("kpi_last_price")),
                column(3, uiOutput("kpi_last_ret")),
                column(3, uiOutput("kpi_vol")),
                column(3, uiOutput("kpi_dd"))
              )
            }
          ),
          hr(),
          h5("Quick Insights"),
          uiOutput("insight_chips")
        )
      ),
      
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          height = "440px",
          full_screen = TRUE,
          bslib::card_header("Price"),
          bslib::card_body(plotlyOutput("price_plot", height = "380px"))
        ),
        bslib::card(
          height = "440px",
          full_screen = TRUE,
          bslib::card_header("Drawdown"),
          bslib::card_body(plotlyOutput("dd_plot", height = "380px"))
        )
      ),
      
      uiOutput("returns_row"),
      
      bslib::card(
        bslib::card_header("Recent observations"),
        bslib::card_body(DTOutput("recent_table"))
      )
    ),
    
    # ---------------------------
    # NEW: Explore tab (upload dataset → expert EDA)
    # ---------------------------
    bslib::nav_panel(
      "Explore",
      
      bslib::card(
        bslib::card_header("Upload a time-series dataset"),
        bslib::card_body(
          p(class = "text-muted",
            "Upload a CSV and map the date/time and value columns. This does not affect your backend DB."
          ),
          fileInput("explore_file", "CSV file", accept = c(".csv")),
          uiOutput("explore_mapping_ui"),
          hr(),
          div(class = "kpi-wrap", uiOutput("explore_kpis_ui"))
        )
      ),
      
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          height = "440px",
          full_screen = TRUE,
          bslib::card_header("Series"),
          bslib::card_body(plotlyOutput("explore_series_plot", height = "380px"))
        ),
        bslib::card(
          height = "440px",
          full_screen = TRUE,
          bslib::card_header("Returns"),
          bslib::card_body(plotlyOutput("explore_returns_plot", height = "380px"))
        )
      ),
      
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          height = "440px",
          full_screen = TRUE,
          bslib::card_header("Rolling volatility"),
          bslib::card_body(plotlyOutput("explore_vol_plot", height = "380px"))
        ),
        bslib::card(
          height = "440px",
          full_screen = TRUE,
          bslib::card_header("ACF"),
          bslib::card_body(plotlyOutput("explore_acf_plot", height = "380px"))
        )
      ),
      
      bslib::card(
        bslib::card_header("Preview"),
        bslib::card_body(DTOutput("explore_preview_table"))
      )
    ),
    
    bslib::nav_panel(
      "Forecast",
      if (has_pred) {
        prediction_ui("pred")
      } else {
        bslib::card(
          bslib::card_header("Point forecast (API)"),
          bslib::card_body(uiOutput("pred_summary"), DTOutput("pred_table"))
        )
      }
    ),
    
    bslib::nav_panel(
      "Diagnostics",
      bslib::card(
        bslib::card_header("Backtest artifacts"),
        bslib::card_body(
          p(class = "text-muted", "Rendered from backend/reports (PNG) and CSV."),
          uiOutput("diag_imgs"),
          hr(),
          h5("Backtest metrics"),
          DTOutput("backtest_table")
        )
      )
    ),
    
    bslib::nav_panel(
      "Insights",
      if (has_insights) insights_ui("insights") else div(class = "text-muted", "Insights module not loaded (R/insights.R).")
    ),
    
    bslib::nav_panel(
      "About",
      if (has_about) about_ui("about") else div(class = "text-muted", "About module not loaded (R/about.R).")
    )
  ),
  
  tags$head(tags$style(HTML("
    /* Top bar */
    .navbar, .bslib-page-navbar {
      background: #111827 !important;
    }
    .navbar .navbar-brand, .bslib-page-navbar .navbar-brand {
      color: #f9fafb !important;
      font-weight: 650;
      letter-spacing: -0.01em;
    }
    .navbar .nav-link, .bslib-page-navbar .nav-link {
      color: rgba(249,250,251,.78) !important;
    }
    .navbar .nav-link.active, .bslib-page-navbar .nav-link.active {
      color: #ffffff !important;
    }

    .app-subtitle { color: rgba(15,23,42,.70); margin-top: -6px; margin-bottom: 12px; }

    /* ✅ Sidebar: make it a nicer “product gray” */
    .app-sidebar, .bslib-sidebar.app-sidebar {
      background: linear-gradient(180deg, #eef2f7 0%, #e3e8f0 100%) !important;
      border-right: 1px solid rgba(15,23,42,.14) !important;
      box-shadow: inset -1px 0 0 rgba(255,255,255,.65);
    }
    .app-sidebar .bslib-sidebar-content {
      padding: 18px 16px !important;
    }
    .app-sidebar h4 {
      color: #0f172a !important;
      font-weight: 800 !important;
      letter-spacing: -0.02em;
    }
    .app-sidebar .text-muted {
      color: rgba(15,23,42,.62) !important;
    }
    .app-sidebar .section-h {
      margin-top: 6px;
      color: rgba(15,23,42,.78) !important;
      font-weight: 700;
    }
    .app-sidebar hr {
      border-top: 1px solid rgba(15,23,42,.12) !important;
      opacity: 1;
    }
    /* Inputs inside sidebar: white “cards” on gray background */
    .app-sidebar .form-control,
    .app-sidebar .selectize-input,
    .app-sidebar .selectize-control.single .selectize-input {
      background: rgba(255,255,255,.92) !important;
      border: 1px solid rgba(15,23,42,.18) !important;
      box-shadow: 0 1px 0 rgba(255,255,255,.70), 0 8px 20px rgba(2,6,23,.05) !important;
    }
    .app-sidebar label, .app-sidebar .control-label {
      color: rgba(15,23,42,.78) !important;
      font-weight: 650 !important;
      font-size: 13px !important;
    }

    /* Cards / controls */
    .card { border-radius: 16px; }
    .card-header { background: transparent; border-bottom: 1px solid rgba(15,23,42,.08); }
    .btn { border-radius: 12px; }

    /* KPI tiles: bigger + stable sizing */
    .kpi-wrap .col-sm-3, .kpi-wrap .col-md-3, .kpi-wrap .col-lg-3 {
      padding-left: 8px !important;
      padding-right: 8px !important;
    }
    .value-box {
      background: #ffffff;
      border: 1px solid rgba(15,23,42,.08);
      border-radius: 16px;
      padding: 14px 14px;
      margin-bottom: 14px;
      box-shadow: 0 6px 20px rgba(2,6,23,.05);
      min-height: 112px;
      display: flex;
      flex-direction: column;
      justify-content: center;
    }
    .value-box .vb-title { color: rgba(15,23,42,.65); font-size: 12px; margin-bottom: 6px; }
    .value-box .vb-value { color: #0f172a; font-weight: 800; font-size: 32px; line-height: 1.05; }
    .value-box .vb-sub { color: rgba(15,23,42,.65); font-size: 12px; margin-top: 6px; }

    .insight-chip {
      display: inline-block; margin: 6px 8px 0 0; padding: 6px 10px;
      border-radius: 999px; background: #fff; border: 1px solid rgba(15,23,42,.08);
      color: rgba(15,23,42,.85); font-size: 12px;
    }

    .dataTables_wrapper { font-size: 12px; }

    .diag-img {
      width: 100%;
      max-width: 1200px;
      border: 1px solid rgba(15,23,42,.08);
      border-radius: 14px;
      background: #ffffff;
      margin: 10px 0 24px 0;
      box-shadow: 0 8px 26px rgba(2,6,23,.06);
    }

    .plotly, .html-widget { max-width: 100%; }
    
    .runbar-wrap{
  position: fixed;
  top: 68px;
  right: 18px;
  z-index: 3000;
}

.runbar{
  display: flex;
  align-items: center;
  gap: 10px;
  padding: 8px 12px;
  border-radius: 10px;
  background: #0b1f35;
  color: #cfe7ff;
  border: 1px solid rgba(255,255,255,.10);
  box-shadow: 0 10px 30px rgba(2,6,23,.25);
}

.runbar .emoji{ font-size: 18px; line-height: 1; }
.runbar .label{ font-weight: 700; letter-spacing: 0.4px; }

  ")))
)

# ---------------------------
# Server
# ---------------------------
server = function(input, output, session) {
  
  api_url = Sys.getenv("PREDICT_API_URL", "http://127.0.0.1:8000/predict")
  
  run_rv = reactiveValues(running = FALSE, cancel = FALSE, frame = 1)
  
  frames = c("🏃", "🏃‍♀️", "🏊", "🧑‍🦽")
  
  observe({
    req(run_rv$running)
    invalidateLater(180, session)
    run_rv$frame = (run_rv$frame %% length(frames)) + 1
  })
  
  output$runbar = renderUI({
    if (!run_rv$running) return(NULL)
    
    tags$div(
      class = "runbar",
      tags$span(class = "emoji", frames[run_rv$frame]),
      tags$span(class = "label", "RUNNING..."),
      actionButton("stop_job", "Stop", class = "btn btn-sm btn-danger")
    )
  })
  
  observeEvent(input$stop_job, {
    run_rv$cancel = TRUE
  }, ignoreInit = TRUE)
  
  
  reports_dir = resolve_reports_dir()
  if (!is.na(reports_dir)) addResourcePath("reports", reports_dir)
  
  data_all_rv = reactiveVal(NULL)
  
  load_or_error = function() {
    if (!exists("load_data", mode = "function")) {
      stop("load_data() not found. Ensure R/utils.R exists and defines load_data().")
    }
    load_data() |>
      mutate(date = as.Date(date), amzn = as.numeric(amzn)) |>
      filter(!is.na(date), !is.na(amzn)) |>
      arrange(date)
  }
  
  observeEvent(TRUE, {
    df = tryCatch(load_or_error(), error = function(e) e)
    if (inherits(df, "error")) {
      showNotification(df$message, type = "error", duration = NULL)
      data_all_rv(data.frame(date = as.Date(character()), amzn = numeric()))
      return()
    }
    data_all_rv(df)
  }, once = TRUE)
  
  if (!has_sidebar) {
    observeEvent(input$refresh_btn, {
      df = tryCatch(load_or_error(), error = function(e) e)
      if (inherits(df, "error")) {
        showNotification(df$message, type = "error", duration = 10)
        return()
      }
      data_all_rv(df)
      showNotification("Data refreshed.", type = "message", duration = 2)
    })
  }
  
  data_all = reactive({
    df = data_all_rv()
    if (is.null(df)) df = data.frame(date = as.Date(character()), amzn = numeric())
    df
  })
  
  controls = reactive(NULL)
  filtered_raw = reactive(NULL)
  
  if (has_sidebar) {
    side = sidebar_server("sidebar", data = data_all)
    filtered_raw = reactive(side$filtered())
    controls = reactive(side$controls())
    
    observeEvent(controls()$refresh_click, ignoreInit = TRUE, {
      df = tryCatch(load_or_error(), error = function(e) e)
      if (inherits(df, "error")) {
        showNotification(df$message, type = "error", duration = 10)
        return()
      }
      data_all_rv(df)
      showNotification("Data refreshed.", type = "message", duration = 2)
    })
  } else {
    filtered_raw = reactive({
      df = data_all()
      if (nrow(df) == 0) return(df)
      
      dr = input$date_range
      if (!is.null(dr) && length(dr) == 2 && all(!is.na(dr))) {
        df = df |> filter(date >= as.Date(dr[[1]]), date <= as.Date(dr[[2]]))
      }
      df
    })
    
    controls = reactive(list(
      view_mode = input$view_mode,
      vol_window = as.integer(input$vol_window),
      show_returns = isTRUE(input$show_returns)
    ))
    
    observeEvent(data_all_rv(), {
      df = data_all()
      if (nrow(df) == 0) return()
      dmin = min(df$date, na.rm = TRUE)
      dmax = max(df$date, na.rm = TRUE)
      updateDateRangeInput(session, "date_range", start = dmin, end = dmax)
      updateDateInput(session, "as_of", value = dmax)
    }, ignoreInit = FALSE)
    
    output$last_updated = renderText({
      paste0("Last updated: ", format(Sys.time(), "%Y-%m-%d %H:%M"))
    })
  }
  
  data_filtered = reactive({
    df = filtered_raw()
    if (is.null(df) || nrow(df) == 0) return(df)
    compute_features_fn(df)
  })
  
  if (has_metrics) metrics_server("metrics", data = data_filtered)
  if (has_about) about_server("about", data_reactive = data_filtered)
  if (has_insights) insights_server("insights", data = data_filtered)
  if (has_pred) prediction_server("pred", api_url = api_url, data = data_all)
  
  output$row_count = renderText({
    df = data_filtered()
    paste0("Rows: ", format(ifelse(is.null(df), 0, nrow(df)), big.mark = ","))
  })
  
  output$insight_chips = renderUI({
    df = data_filtered()
    if (is.null(df) || nrow(df) < 80) return(tags$span(class = "text-muted", "Not enough data in view for insights."))
    
    last = df |> tail(1)
    vol60 = last$roll_sd_60[[1]]
    mom20 = df$ret_1 |> tail(20) |> sum(na.rm = TRUE)
    dd = last$drawdown[[1]]
    
    chips = list(
      tags$span(class = "insight-chip",
                paste0("Regime vol(60d): ", ifelse(is.na(vol60), "—", sprintf("%.4f", vol60)))
      ),
      tags$span(class = "insight-chip",
                paste0("Momentum(20d): ", ifelse(is.na(mom20), "—", sprintf("%.2f%%", (exp(mom20) - 1) * 100)))
      ),
      tags$span(class = "insight-chip",
                paste0("Current drawdown: ", ifelse(is.na(dd), "—", sprintf("%.2f%%", dd * 100)))
      )
    )
    do.call(tagList, chips)
  })
  
  output$price_plot = renderPlotly({
    df = data_filtered()
    if (is.null(df) || nrow(df) == 0) return(plotly_empty_msg())
    log_scale = isTRUE(controls()$view_mode == "log")
    plot_price_fn(df, log_scale = log_scale)
  })
  
  output$returns_row = renderUI({
    show_it = if (has_sidebar) isTRUE(controls()$show_returns) else isTRUE(input$show_returns)
    if (!show_it) return(NULL)
    
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        height = "420px",
        full_screen = TRUE,
        bslib::card_header("Returns"),
        bslib::card_body(plotlyOutput("ret_plot", height = "340px"))
      ),
      bslib::card(
        height = "420px",
        full_screen = TRUE,
        bslib::card_header("Rolling volatility"),
        bslib::card_body(plotlyOutput("vol_plot", height = "340px"))
      )
    )
  })
  
  output$ret_plot = renderPlotly({
    df = data_filtered()
    if (is.null(df) || nrow(df) == 0) return(plotly_empty_msg())
    plot_returns_fn(df)
  })
  
  output$vol_plot = renderPlotly({
    df = data_filtered()
    if (is.null(df) || nrow(df) == 0) return(plotly_empty_msg())
    w = as.integer(controls()$vol_window %||% 60)
    plot_roll_vol_fn(df, window = w)
  })
  
  output$dd_plot = renderPlotly({
    df = data_filtered()
    if (is.null(df) || nrow(df) == 0) return(plotly_empty_msg())
    plot_drawdown_fn(df)
  })
  
  output$recent_table = renderDT({
    df = data_filtered()
    if (is.null(df) || nrow(df) == 0) {
      return(DT::datatable(data.frame(note = "No data in selected range."), rownames = FALSE))
    }
    
    out = df |>
      tail(40) |>
      select(date, amzn, ret_1, roll_sd_20, roll_sd_60, drawdown) |>
      mutate(
        amzn = round(amzn, 2),
        ret_1 = round(ret_1, 6),
        roll_sd_20 = round(roll_sd_20, 6),
        roll_sd_60 = round(roll_sd_60, 6),
        drawdown = round(drawdown, 4)
      )
    
    DT::datatable(out, rownames = FALSE, options = list(pageLength = 10, dom = "tip", scrollX = TRUE))
  })
  
  output$download_filtered = downloadHandler(
    filename = function() paste0("amzn_filtered_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      df = data_filtered()
      if (is.null(df) || nrow(df) == 0) {
        write.csv(data.frame(), file, row.names = FALSE)
      } else {
        write.csv(df |> select(date, amzn), file, row.names = FALSE)
      }
    }
  )
  
  output$diag_imgs = renderUI({
    if (is.na(reports_dir)) {
      return(div(class = "text-muted",
                 "Could not find reports directory. Expected one of: reports/, backend/reports/, ../backend/reports/."
      ))
    }
    imgs = c("backtest_rmse_by_horizon.png", "actual_vs_pred.png")
    tagList(lapply(imgs, function(f) {
      tags$div(style = "margin: 12px 0;",
               tags$img(src = paste0("reports/", f), class = "diag-img")
      )
    }))
  })
  
  output$backtest_table = renderDT({
    if (is.na(reports_dir)) return(DT::datatable(data.frame(note = "reports dir not found."), rownames = FALSE))
    
    csv_path = file.path(reports_dir, "backtest_metrics.csv")
    if (!file.exists(csv_path)) {
      return(DT::datatable(data.frame(note = "backtest_metrics.csv not found in reports dir."), rownames = FALSE))
    }
    
    m = readr::read_csv(csv_path, show_col_types = FALSE)
    DT::datatable(m, rownames = FALSE, options = list(pageLength = 10, dom = "tip", scrollX = TRUE))
  })
  
  # ---------------------------
  # Explore (upload) — expert EDA
  # ---------------------------
  explore_raw = reactive({
    fi = input$explore_file
    if (is.null(fi) || is.null(fi$datapath) || !file.exists(fi$datapath)) return(NULL)
    
    df = tryCatch(utils::read.csv(fi$datapath, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) e)
    if (inherits(df, "error")) {
      showNotification(paste0("Explore upload error: ", df$message), type = "error", duration = 8)
      return(NULL)
    }
    if (nrow(df) == 0) return(NULL)
    df
  })
  
  output$explore_mapping_ui = renderUI({
    df = explore_raw()
    if (is.null(df)) return(NULL)
    
    cols = names(df)
    guess_date = cols[which.min(!grepl("date|time", tolower(cols)))]
    guess_date = cols[which(grepl("date|time", tolower(cols)))[1]] %||% cols[1]
    guess_val = setdiff(cols, guess_date)[1] %||% cols[2]
    
    tagList(
      fluidRow(
        column(6, selectInput("explore_date_col", "Date/time column", choices = cols, selected = guess_date)),
        column(6, selectInput("explore_value_col", "Value column", choices = cols, selected = guess_val))
      ),
      checkboxInput("explore_sort_unique", "Sort by time and drop duplicate timestamps", value = TRUE)
    )
  })
  
  explore_clean = reactive({
    df0 = explore_raw()
    req(!is.null(df0))
    req(input$explore_date_col, input$explore_value_col)
    
    dcol = input$explore_date_col
    vcol = input$explore_value_col
    
    d = df0[[dcol]]
    # robust parse: try ymd/ymd_hms/parse_date_time on strings
    dt = suppressWarnings(as.POSIXct(d, tz = "UTC"))
    if (all(is.na(dt))) {
      dt = suppressWarnings(lubridate::ymd_hms(d, quiet = TRUE, tz = "UTC"))
    }
    if (all(is.na(dt))) {
      dt = suppressWarnings(lubridate::ymd(d, quiet = TRUE, tz = "UTC"))
    }
    if (all(is.na(dt))) {
      dt = suppressWarnings(lubridate::parse_date_time(d, orders = c("ymd HMS", "ymd HM", "ymd", "mdy HMS", "mdy HM", "mdy", "dmy HMS", "dmy HM", "dmy"), tz = "UTC"))
    }
    
    y = suppressWarnings(as.numeric(df0[[vcol]]))
    
    out = data.frame(
      ts = dt,
      y = y
    ) |>
      filter(!is.na(ts), is.finite(y)) |>
      arrange(ts)
    
    if (isTRUE(input$explore_sort_unique) && nrow(out) > 0) {
      out = out |> distinct(ts, .keep_all = TRUE)
    }
    
    # engineered features (generic)
    out = out |>
      mutate(
        date = as.Date(ts),
        y_pos = y > 0,
        log_y = ifelse(y_pos, log(y), NA_real_),
        ret_1 = ifelse(y_pos, log_y - lag(log_y), y - lag(y)),
        roll_sd_20 = slider::slide_dbl(ret_1, sd, .before = 19, .complete = TRUE, na.rm = TRUE),
        roll_sd_60 = slider::slide_dbl(ret_1, sd, .before = 59, .complete = TRUE, na.rm = TRUE),
        peak = cummax(y),
        drawdown = y / peak - 1
      )
    
    out
  })
  
  explore_kpis = reactive({
    d = explore_clean()
    if (is.null(d) || nrow(d) == 0) return(NULL)
    
    n = nrow(d)
    tmin = min(d$ts, na.rm = TRUE)
    tmax = max(d$ts, na.rm = TRUE)
    
    # frequency inference
    dt_days = as.numeric(diff(sort(unique(d$ts)))) / 86400
    med = suppressWarnings(stats::median(dt_days, na.rm = TRUE))
    freq = if (!is.finite(med)) "—" else {
      if (abs(med - 1) < 0.2) "≈ 1 day"
      else if (abs(med - 7) < 1.0) "≈ 1 week"
      else if (med >= 28 && med <= 31) "≈ 1 month"
      else paste0("≈ ", sprintf("%.2f", med), " days")
    }
    
    miss_ret = mean(!is.finite(d$ret_1), na.rm = TRUE) * 100
    last_y = d$y[[n]]
    last_dd = d$drawdown[[n]]
    vol20 = d$roll_sd_20[[n]]
    vol60 = d$roll_sd_60[[n]]
    
    list(
      n = n,
      span = paste0(format(as.Date(tmin)), " → ", format(as.Date(tmax))),
      freq = freq,
      miss_ret = miss_ret,
      last_y = last_y,
      dd = last_dd,
      vol20 = vol20,
      vol60 = vol60
    )
  })
  
  output$explore_kpis_ui = renderUI({
    k = explore_kpis()
    if (is.null(k)) return(NULL)
    
    # lightweight tiles (reuse existing value_box)
    fluidRow(
      column(3, value_box("Rows", format(k$n, big.mark = ","), "after parsing & cleaning")),
      column(3, value_box("Span", k$span, "min → max timestamp")),
      column(3, value_box("Inferred cadence", k$freq, "median Δt")),
      column(3, value_box("Missing returns", paste0(sprintf("%.1f", k$miss_ret), "%"), "non-finite ret_1"))
    )
  })
  
  output$explore_series_plot = renderPlotly({
    d = explore_clean()
    if (is.null(d) || nrow(d) == 0) return(plotly_empty_msg("Upload a dataset to view plots."))
    
    plot_ly(
      d, x = ~ts, y = ~y, type = "scatter", mode = "lines",
      hovertemplate = "t: %{x}<br>y: %{y}<extra></extra>"
    ) |>
      layout(
        template = "plotly_white",
        title = list(text = "Time series", x = 0, xanchor = "left"),
        xaxis = list(title = ""),
        yaxis = list(title = "value"),
        margin = list(l = 60, r = 20, t = 55, b = 50)
      )
  })
  
  output$explore_returns_plot = renderPlotly({
    d = explore_clean()
    if (is.null(d) || nrow(d) == 0) return(plotly_empty_msg("Upload a dataset to view plots."))
    
    r = d |> filter(is.finite(ret_1))
    if (nrow(r) < 5) return(plotly_empty_msg("Not enough return observations to plot."))
    
    plot_ly(
      r, x = ~ts, y = ~ret_1, type = "scatter", mode = "lines",
      hovertemplate = "t: %{x}<br>ret_1: %{y:.6f}<extra></extra>"
    ) |>
      layout(
        template = "plotly_white",
        title = list(text = "Returns (log if y>0 else diff)", x = 0, xanchor = "left"),
        xaxis = list(title = ""),
        yaxis = list(title = "ret_1"),
        margin = list(l = 60, r = 20, t = 55, b = 50)
      )
  })
  
  output$explore_vol_plot = renderPlotly({
    d = explore_clean()
    if (is.null(d) || nrow(d) == 0) return(plotly_empty_msg("Upload a dataset to view plots."))
    
    v = d |> filter(is.finite(roll_sd_60))
    if (nrow(v) < 5) return(plotly_empty_msg("Not enough points to compute rolling volatility."))
    
    plot_ly(
      v, x = ~ts, y = ~roll_sd_60, type = "scatter", mode = "lines",
      hovertemplate = "t: %{x}<br>roll_sd_60: %{y:.6f}<extra></extra>"
    ) |>
      layout(
        template = "plotly_white",
        title = list(text = "Rolling volatility (60)", x = 0, xanchor = "left"),
        xaxis = list(title = ""),
        yaxis = list(title = "sd(ret_1)"),
        margin = list(l = 60, r = 20, t = 55, b = 50)
      )
  })
  
  output$explore_acf_plot = renderPlotly({
    d = explore_clean()
    if (is.null(d) || nrow(d) == 0) return(plotly_empty_msg("Upload a dataset to view plots."))
    
    x = d$ret_1
    x = x[is.finite(x)]
    if (length(x) < 80) return(plotly_empty_msg("Need ~80+ finite returns for a stable ACF."))
    
    lag_max = min(80, length(x) - 2)
    a = stats::acf(x, plot = FALSE, lag.max = lag_max)
    acf_df = data.frame(lag = as.numeric(a$lag), acf = as.numeric(a$acf)) |>
      dplyr::filter(lag > 0)
    
    plot_ly(
      acf_df, x = ~lag, y = ~acf, type = "scatter", mode = "lines",
      hovertemplate = "lag: %{x}<br>acf: %{y:.3f}<extra></extra>"
    ) |>
      layout(
        template = "plotly_white",
        title = list(text = "ACF(ret_1)", x = 0, xanchor = "left"),
        xaxis = list(title = "lag"),
        yaxis = list(title = "acf"),
        margin = list(l = 60, r = 20, t = 55, b = 50)
      )
  })
  
  output$explore_preview_table = renderDT({
    df0 = explore_raw()
    if (is.null(df0)) return(DT::datatable(data.frame(note = "Upload a CSV to preview."), rownames = FALSE))
    DT::datatable(head(df0, 200), rownames = FALSE, options = list(pageLength = 10, dom = "tip", scrollX = TRUE))
  })
}

shinyApp(ui = ui, server = server)

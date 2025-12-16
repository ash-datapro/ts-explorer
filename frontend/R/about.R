# about.R — About module (time-series / forecasting, light theme)

suppressPackageStartupMessages({
  library(shiny)
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(readr)
})

about_ui = function(id) {
  ns = NS(id)
  
  tagList(
      h2("About"),
      div(
        class = "text-muted",
        tags$b("A production-style dashboard for leakage-safe time-series EDA, walk-forward backtesting, and API-based forecasting.")
      ),
      div(
        class = "text-muted",
        tags$p(tags$b("Machine Learning behind-the-scenes:")),
        tags$ol(
          tags$li("Sets up a reproducible training run (fixed random seed, forecast horizons, rolling backtest settings, tuning budget) and pulls a clean time-ordered price series from Postgres using env-based credentials."),
          tags$li("Converts prices into returns and builds a leak-free feature table using only information available at each time point (lagged returns, rolling averages/volatility, momentum-style sums, and simple calendar signals)."),
          tags$li("Creates one supervised learning problem per horizon (1, 5, 20 days): features at time t predict the return at time t + h (direct multi-horizon forecasting, not iterative)."),
          tags$li("Evaluates with rolling-origin cross-validation that repeatedly trains on the past and tests on the next h steps, moving forward through time to simulate live use."),
          tags$li("Establishes a tough baseline by predicting zero return everywhere and computes RMSE/MAE across folds so the model has to beat “do nothing.”"),
          tags$li("Builds a production-style workflow: a preprocessing pipeline (impute, encode calendar vars, drop constant columns, scale numerics) plus a tunable XGBoost regression model."),
          tags$li("Tunes hyperparameters inside the rolling backtest using a fixed-size random grid, selects the best setting by lowest RMSE, and reports MAE as well."),
          tags$li("Trains final models on all available data for each horizon, then saves deployable artifacts: metrics CSV, plots, and a serialized model bundle containing the fitted models plus metadata, settings, and best params."),
          tags$li("Produces quick diagnostics: a baseline-vs-model RMSE comparison by horizon and an actual-vs-predicted plot to spot bias, shrinkage to zero, or noise-fitting.")
        )
      ),
    br(),
    
    fluidRow(
      column(
        3,
        tags$div(
          class = "value-box",
          tags$div(class = "vb-title", "App"),
          tags$div(textOutput(ns("app_name"), inline = TRUE), class = "vb-value"),
          tags$div(textOutput(ns("build_stamp")), class = "vb-sub")
        )
      ),
      column(
        3,
        tags$div(
          class = "value-box",
          tags$div(class = "vb-title", "Runtime"),
          tags$div(textOutput(ns("r_version"), inline = TRUE), class = "vb-value"),
          tags$div(textOutput(ns("shiny_version")), class = "vb-sub")
        )
      ),
      column(
        3,
        tags$div(
          class = "value-box",
          tags$div(class = "vb-title", "API Health"),
          tags$div(textOutput(ns("api_status"), inline = TRUE), class = "vb-value"),
          tags$div(textOutput(ns("api_latency")), class = "vb-sub")
        )
      ),
      column(
        3,
        tags$div(
          class = "value-box",
          tags$div(class = "vb-title", "Rows (current view)"),
          tags$div(textOutput(ns("rows_visible"), inline = TRUE), class = "vb-value"),
          tags$div(textOutput(ns("rows_stamp")), class = "vb-sub")
        )
      )
    ),
    
    br(),
    
    tags$details(
      open = TRUE,
      tags$summary("Model & target"),
      tags$ul(
        tags$li(tags$strong("Target:"), " h-day-ahead log return (direct per horizon)."),
        tags$li(tags$strong("Horizons:"), " 1, 5, 20 trading days."),
        tags$li(tags$strong("Leakage control:"), " lag/rolling features computed strictly from past.")
      ),
      tags$p(
        class = "text-muted",
        "Tip: In production, add price-level baselines (random walk/drift/ETS/ARIMA) alongside return models for monitoring."
      )
    ),
    
    tags$details(
      tags$summary("Endpoints"),
      tags$pre(style = "white-space: pre-wrap; font-size: 12px;",
               textOutput(ns("endpoints")))
    ),
    
    tags$details(
      tags$summary("Model metadata (/meta)"),
      verbatimTextOutput(ns("meta_json"))
    ),
    
    tags$details(
      tags$summary("Backtest metrics (reports/backtest_metrics.csv)"),
      tableOutput(ns("metrics_table")),
      tags$p(class = "text-muted",
             "This table is read from the training artifact; keep it under version control or store it in an artifacts bucket.")
    ),
    
    tags$details(
      tags$summary("Data dictionary"),
      tableOutput(ns("schema_table"))
    )
  )
}

about_server = function(id, data_reactive = NULL) {
  moduleServer(id, function(input, output, session) {
    
    output$app_name = renderText("Time-Series Dashboard")
    
    output$build_stamp = renderText({
      paste0("Build: ", format(Sys.time(), "%Y-%m-%d %H:%M"))
    })
    
    output$r_version = renderText({
      paste0("R ", getRversion())
    })
    
    output$shiny_version = renderText({
      v = tryCatch(as.character(utils::packageVersion("shiny")), error = function(e) NA_character_)
      paste0("shiny ", v)
    })
    
    api_url = reactive({
      Sys.getenv("PREDICT_API_URL", "http://127.0.0.1:8000/predict")
    })
    
    output$endpoints = renderText({
      api = api_url()
      health = sub("/predict$", "/health", api)
      meta = sub("/predict$", "/meta", api)
      paste0(
        "PREDICT_API_URL = ", api, "\n",
        "HEALTH          = ", health, "\n",
        "META            = ", meta, "\n"
      )
    })
    
    output$api_status = renderText({
      api = api_url()
      health = sub("/predict$", "/health", api)
      ok = tryCatch({
        res = httr::GET(health, timeout(2))
        httr::status_code(res) == 200
      }, error = function(e) FALSE)
      if (ok) "OK" else "Down"
    })
    
    output$api_latency = renderText({
      api = api_url()
      health = sub("/predict$", "/health", api)
      t0 = Sys.time()
      ms = tryCatch({
        res = httr::GET(health, timeout(2))
        as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
      }, error = function(e) NA_real_)
      if (is.na(ms)) "Timeout" else paste0(round(ms), " ms")
    })
    
    output$meta_json = renderText({
      api = api_url()
      meta = sub("/predict$", "/meta", api)
      txt = tryCatch({
        res = httr::GET(meta, timeout(3))
        if (httr::status_code(res) != 200) return("Meta unavailable (API not ready or /meta not implemented).")
        httr::content(res, as = "text", encoding = "UTF-8")
      }, error = function(e) "Meta unavailable (API not reachable).")
      txt
    })
    
    output$rows_visible = renderText({
      if (is.null(data_reactive)) return("—")
      df = tryCatch(data_reactive(), error = function(e) NULL)
      if (is.null(df)) return("—")
      format(nrow(df), big.mark = ",")
    })
    
    output$rows_stamp = renderText({
      paste0("As of ", format(Sys.time(), "%Y-%m-%d %H:%M"))
    })
    
    output$schema_table = renderTable({
      data.frame(
        Field = c("date", "amzn"),
        Type = c("Date", "numeric"),
        Description = c("Trading day date", "AMZN price (close)"),
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, bordered = TRUE, spacing = "s")
    
    output$metrics_table = renderTable({
      path = file.path("~/Desktop/Project/ts-explorer/backend/reports", "backtest_metrics.csv")
      if (!file.exists(path)) {
        return(data.frame(note = "reports/backtest_metrics.csv not found"))
      }
      m = readr::read_csv(path, show_col_types = FALSE)
      m |> as.data.frame()
    }, striped = TRUE, bordered = TRUE, spacing = "s")
  })
}

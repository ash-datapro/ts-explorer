# prediction.R — Prediction module (time-series forecasting via Plumber API)
# Clean forecast console: history plot + shaded horizon + advanced API details.

suppressPackageStartupMessages({
  library(shiny)
  library(httr)
  library(jsonlite)
  library(plotly)
  library(scales)
  library(dplyr)
  library(lubridate)
})

# UI ---------------------------------------------------------------------------
prediction_ui = function(id) {
  ns = NS(id)
  
  tagList(
    h3("Forecast"),
    
    # Top controls (compact)
    fluidRow(
      column(
        8,
        selectInput(
          ns("preset"),
          label = NULL,
          choices = c(
            "Choose an example…" = "",
            "Latest available (recommended)" = "latest",
            "1 week ago (as-of)" = "wk_ago",
            "1 month ago (as-of)" = "mo_ago"
          ),
          width = "100%"
        )
      ),
      column(
        4,
        div(style = "text-align:right;",
            actionButton(ns("reset"), "Reset"),
            actionButton(ns("predict"), "Predict", class = "btn-primary"))
      )
    ),
    br(),
    
    fluidRow(
      column(
        7,
        fluidRow(
          column(6, dateInput(ns("as_of"), "As-of date", value = Sys.Date(), format = "yyyy-mm-dd")),
          column(6, selectInput(ns("horizon"), "Horizon (days)", choices = c("1" = 1, "5" = 5, "20" = 20), selected = 5))
        ),
        checkboxInput(ns("predict_all"), "Return all horizons (1/5/20) in one call", value = FALSE)
      ),
      column(
        5,
        tags$div(
          class = "value-box",
          div(class = "vb-title", textOutput(ns("status_title"), inline = TRUE)),
          div(class = "vb-value", textOutput(ns("status_value"), inline = TRUE)),
          div(class = "vb-sub", textOutput(ns("status_sub"), inline = TRUE))
        )
      )
    ),
    
    br(),
    
    # Forecast context plot (history + shaded horizon)
    plotlyOutput(ns("forecast_plot"), height = "420px"),
    
    br(),
    
    fluidRow(
      column(
        12,
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("predict_all")),
          h4("Multi-horizon output"),
          DT::DTOutput(ns("pred_table"))
        )
      )
    ),
    
    br(),
    
    bslib::accordion(
      bslib::accordion_panel(
        "Advanced (API details)",
        tags$small(class = "text-muted", "API: ", textOutput(ns("api_label"), inline = TRUE)),
        br(),
        tags$details(
          open = FALSE,
          tags$summary("Request payload (JSON)"),
          tags$pre(style = "white-space: pre-wrap; font-size: 12px;",
                   textOutput(ns("payload_preview")))
        ),
        br(),
        tags$details(
          open = FALSE,
          tags$summary("Raw response"),
          verbatimTextOutput(ns("raw_response"))
        )
      )
    )
  )
}

# SERVER -----------------------------------------------------------------------
prediction_server = function(id, api_url = NULL, data = NULL) {
  moduleServer(id, function(input, output, session) {
    
    api = reactive({
      if (!is.null(api_url)) return(api_url)
      Sys.getenv("PREDICT_API_URL", "http://127.0.0.1:8000/predict")
    })
    output$api_label = renderText(api())
    
    # data reactive (optional but recommended for plotting)
    data_rx = reactive({
      if (is.null(data)) return(NULL)
      df = data()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df |>
        mutate(date = as.Date(date), amzn = as.numeric(amzn)) |>
        filter(!is.na(date), !is.na(amzn)) |>
        arrange(date)
    })
    
    # Presets should respect available data range if we have it
    set_preset = function(code) {
      df = data_rx()
      if (!is.null(df)) {
        dmax = max(df$date, na.rm = TRUE)
        if (identical(code, "latest")) updateDateInput(session, "as_of", value = dmax)
        if (identical(code, "wk_ago")) updateDateInput(session, "as_of", value = dmax - 7)
        if (identical(code, "mo_ago")) updateDateInput(session, "as_of", value = dmax - 30)
      } else {
        if (identical(code, "latest")) updateDateInput(session, "as_of", value = Sys.Date())
        if (identical(code, "wk_ago")) updateDateInput(session, "as_of", value = Sys.Date() - 7)
        if (identical(code, "mo_ago")) updateDateInput(session, "as_of", value = Sys.Date() - 30)
      }
    }
    
    observeEvent(input$preset, ignoreInit = TRUE, {
      set_preset(input$preset)
    })
    
    observeEvent(input$reset, {
      updateSelectInput(session, "preset", selected = "")
      set_preset("latest")
      updateSelectInput(session, "horizon", selected = 5)
      updateCheckboxInput(session, "predict_all", value = FALSE)
    })
    
    assemble_body = reactive({
      body = list(as_of = as.character(as.Date(input$as_of)))
      if (!isTRUE(input$predict_all)) body$horizon = as.integer(input$horizon)
      body
    })
    
    output$payload_preview = renderText({
      jsonlite::toJSON(assemble_body(), auto_unbox = TRUE, pretty = TRUE)
    })
    
    pred_state = reactiveVal(NULL)
    err_state = reactiveVal(NULL)
    
    call_api = function(body) {
      tryCatch({
        httr::POST(
          url = api(),
          body = jsonlite::toJSON(body, auto_unbox = TRUE),
          encode = "raw",
          httr::add_headers("Content-Type" = "application/json"),
          httr::timeout(10)
        )
      }, error = function(e) e)
    }
    
    render_error = function(msg) {
      err_state(msg)
      pred_state(NULL)
    }
    
    render_success = function(parsed) {
      err_state(NULL)
      pred_state(parsed)
    }
    
    output$raw_response = renderPrint({
      e = err_state()
      if (!is.null(e)) return(e)
      x = pred_state()
      if (is.null(x)) return("No prediction yet.")
      x
    })
    
    # Status box (simple + clean)
    output$status_title = renderText({
      if (!is.null(err_state())) "Status"
      else "Result"
    })
    
    output$status_value = renderText({
      e = err_state()
      if (!is.null(e)) return("Error")
      x = pred_state()
      if (is.null(x)) return("—")
      if (isTRUE(input$predict_all)) "OK (multi-horizon)" else paste0("OK (h=", input$horizon, ")")
    })
    
    output$status_sub = renderText({
      e = err_state()
      if (!is.null(e)) return(e)
      x = pred_state()
      if (is.null(x)) return("Submit a request to populate forecast.")
      paste0("as_of = ", x$as_of, "  •  target = log return @ horizon")
    })
    
    # Helper: pick predicted log-return for selected horizon
    pick_pred = function(x) {
      preds = x$prediction
      if (is.null(preds)) return(list(h = NA_integer_, rhat = NA_real_))
      
      if (!isTRUE(input$predict_all)) {
        h = as.integer(input$horizon)
        key = paste0("h", h)
        rhat = suppressWarnings(as.numeric(preds[[key]]))
        return(list(h = h, rhat = rhat))
      }
      
      # multi-horizon: prefer selected horizon if present, else h5, else first
      h = as.integer(input$horizon)
      key = paste0("h", h)
      rhat = suppressWarnings(as.numeric(preds[[key]]))
      if (!is.finite(rhat)) {
        rhat = suppressWarnings(as.numeric(preds[["h5"]]))
        h = 5L
      }
      if (!is.finite(rhat)) {
        rhat = suppressWarnings(as.numeric(preds[[1]]))
      }
      list(h = h, rhat = rhat)
    }
    
    # Forecast plot: history + as_of marker + shaded horizon window + implied price line
    output$forecast_plot = renderPlotly({
      df = data_rx()
      
      # If no data available (shouldn’t happen in your setup, but safe)
      if (is.null(df) || nrow(df) < 5) {
        return(plot_ly() |>
                 layout(template = "plotly_white",
                        annotations = list(list(
                          text = "No series data available for forecast context.",
                          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                          showarrow = FALSE
                        ))))
      }
      
      # choose as_of
      as_of = as.Date(input$as_of)
      if (is.na(as_of)) as_of = max(df$date)
      
      # use last available trading day <= as_of
      as_of_eff = max(df$date[df$date <= as_of], na.rm = TRUE)
      if (!is.finite(as_of_eff)) as_of_eff = max(df$date)
      
      # prediction state (optional)
      x = pred_state()
      rhat = NA_real_
      h = as.integer(input$horizon)
      cur_price = NA_real_
      
      if (!is.null(x) && is.null(err_state())) {
        picked = pick_pred(x)
        h = picked$h
        rhat = picked$rhat
        cur_price = suppressWarnings(as.numeric(x$current_price))
      } else {
        # fallback context price from series at as_of_eff
        cur_price = df$amzn[match(as_of_eff, df$date)]
        cur_price = suppressWarnings(as.numeric(cur_price))
      }
      
      # compute implied price at horizon end
      end_date = as_of_eff + days(h)
      implied_price = if (is.finite(cur_price) && is.finite(rhat)) cur_price * exp(rhat) else NA_real_
      implied_move_pct = if (is.finite(rhat)) (exp(rhat) - 1) * 100 else NA_real_
      
      # Build a view window: last 180 trading days before as_of for clarity
      df_view = df |> filter(date <= as_of_eff) |> tail(260)
      y_max = max(df_view$amzn, na.rm = TRUE)
      y_min = min(df_view$amzn, na.rm = TRUE)
      
      p = plot_ly(df_view, x = ~date, y = ~amzn, type = "scatter", mode = "lines",
                  name = "Price",
                  hovertemplate = "Date: %{x}<br>Price: %{y:$,.2f}<extra></extra>") |>
        layout(
          template = "plotly_white",
          title = list(text = "Price history with forecast window"),
          margin = list(l = 60, r = 20, t = 60, b = 50),
          xaxis = list(title = ""),
          yaxis = list(title = "price", tickprefix = "$")
        )
      
      # as-of vertical line
      p = p |>
        add_segments(
          x = as_of_eff, xend = as_of_eff,
          y = y_min, yend = y_max,
          inherit = FALSE,
          name = "as-of",
          line = list(dash = "dot"),
          showlegend = FALSE,
          hoverinfo = "skip"
        )
      
      # shaded forecast window (as_of -> end_date)
      p = p |>
        layout(
          shapes = list(
            list(
              type = "rect",
              xref = "x", yref = "paper",
              x0 = as_of_eff, x1 = end_date,
              y0 = 0, y1 = 1,
              fillcolor = "rgba(37, 99, 235, 0.10)", # accent blue (light)
              line = list(width = 0)
            )
          )
        )
      
      # implied price marker + annotation (only if we have a prediction)
      if (is.finite(implied_price)) {
        p = p |>
          add_markers(
            x = end_date, y = implied_price,
            name = "Implied price",
            marker = list(size = 10),
            hovertemplate = paste0(
              "Horizon end: %{x}<br>",
              "Implied price: $%{y:,.2f}<br>",
              "Implied move: ", sprintf("%.2f", implied_move_pct), "%<extra></extra>"
            )
          ) |>
          layout(
            annotations = list(
              list(
                x = end_date, y = implied_price,
                text = paste0("Implied: ", scales::dollar(implied_price), " (", sprintf("%.2f", implied_move_pct), "%)"),
                showarrow = TRUE, arrowhead = 2, ax = 40, ay = -40
              )
            )
          )
      }
      
      p
    })
    
    # Multi-horizon table (only when predict_all)
    output$pred_table = DT::renderDT({
      x = pred_state()
      if (is.null(x) || is.null(x$prediction)) {
        return(DT::datatable(data.frame(note = "No multi-horizon prediction yet."), rownames = FALSE,
                             options = list(dom = "t")))
      }
      
      preds = x$prediction
      tb = tibble::tibble(
        horizon = names(preds),
        pred_log_return = as.numeric(unlist(preds))
      ) |>
        mutate(
          horizon_days = as.integer(gsub("^h", "", horizon)),
          implied_move_pct = (exp(pred_log_return) - 1) * 100
        ) |>
        select(horizon_days, pred_log_return, implied_move_pct) |>
        arrange(horizon_days) |>
        mutate(
          pred_log_return = round(pred_log_return, 6),
          implied_move_pct = round(implied_move_pct, 3)
        )
      
      DT::datatable(tb, rownames = FALSE, options = list(dom = "t", pageLength = 10))
    })
    
    # Predict click -----------------------------------------------------------
    observeEvent(input$predict, {
      as_of = as.Date(input$as_of)
      if (is.na(as_of)) {
        render_error("Invalid as_of date. Use YYYY-MM-DD.")
        return()
      }
      
      body = assemble_body()
      res = call_api(body)
      
      if (inherits(res, "error")) {
        render_error(res$message)
        return()
      }
      
      if (httr::status_code(res) != 200) {
        txt = httr::content(res, as = "text", encoding = "UTF-8")
        render_error(paste0("API error (", httr::status_code(res), "): ", txt))
        return()
      }
      
      parsed = httr::content(res, as = "parsed", simplifyVector = TRUE)
      if (is.null(parsed$prediction)) {
        render_error("Invalid API response: missing prediction field.")
        return()
      }
      
      render_success(parsed)
    })
    
    # initialize preset
    observeEvent(TRUE, { set_preset("latest") }, once = TRUE)
  })
}

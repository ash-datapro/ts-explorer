# insights.R — Insights module (time-series diagnostics, light theme)

suppressPackageStartupMessages({
  library(shiny)
  library(plotly)
  library(dplyr)
  library(lubridate)
  library(stats)
})

insights_ui = function(id) {
  ns = NS(id)
  
  tagList(
    h2("Insights"),
    fluidRow(
      column(6, tags$span(class = "text-muted", "Relationships, regime proxies, distribution checks")),
      column(6, div(style = "text-align:right;",
                    actionButton(ns("refresh"), "↻ Recompute (sampled)")))
    ),
    br(),
    
    fluidRow(
      column(6, h4("Correlation (engineered numeric features)"),
             plotlyOutput(ns("corr"), height = "360px"),
             uiOutput(ns("corr_note"))),
      column(6, h4("Return distribution"),
             plotlyOutput(ns("ret_hist"), height = "360px"))
    ),
    br(),
    
    fluidRow(
      column(6, h4("Avg return by day-of-week"),
             plotlyOutput(ns("dow_ret"), height = "340px")),
      column(6, h4("Volatility regime (60d SD quantiles)"),
             plotlyOutput(ns("vol_regime"), height = "340px"))
    ),
    br(),
    
    fluidRow(
      column(12, h4("Autocorrelation (returns and squared returns)"),
             plotlyOutput(ns("acf_panel"), height = "380px"),
             tags$small(class = "text-muted",
                        "ACF is computed on the current view; interpret cautiously for non-stationary regimes."))
    )
  )
}

insights_server = function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    sampled = reactive({
      df = data()
      if (is.null(df) || nrow(df) == 0) return(df)
      if (nrow(df) > 3000) df = df[sample(seq_len(nrow(df)), 3000), , drop = FALSE]
      df
    })
    observeEvent(input$refresh, { invisible(sampled()) })
    
    # Expect the app to pass an engineered dataset with at least:
    # date, amzn, ret_1, roll_sd_20, roll_sd_60, drawdown
    output$corr = renderPlotly({
      df = sampled()
      req(!is.null(df), nrow(df) > 5)
      
      num_df = df |> select(where(is.numeric))
      if (ncol(num_df) < 2) return(NULL)
      
      cor_mat = tryCatch(cor(num_df, use = "pairwise.complete.obs"), error = function(e) NULL)
      if (is.null(cor_mat)) return(NULL)
      
      plot_ly(
        x = colnames(cor_mat), y = rownames(cor_mat),
        z = cor_mat, type = "heatmap", colorscale = "Viridis",
        hovertemplate = "X: %{x}<br>Y: %{y}<br>ρ: %{z:.2f}<extra></extra>"
      ) |>
        layout(template = "plotly_white", xaxis = list(title = ""), yaxis = list(title = ""))
    })
    
    output$corr_note = renderUI({
      df = sampled()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      k = sum(sapply(df, is.numeric))
      HTML(sprintf("<span class='text-muted'>Using %d numeric columns in current view.</span>", k))
    })
    
    output$ret_hist = renderPlotly({
      df = sampled()
      req(!is.null(df))
      if (!"ret_1" %in% names(df)) return(NULL)
      
      r = df$ret_1
      r = r[is.finite(r)]
      
      if (length(r) < 30) return(NULL)
      
      plot_ly(x = r, type = "histogram", nbinsx = 60,
              hovertemplate = "ret: %{x:.5f}<br>count: %{y}<extra></extra>") |>
        layout(template = "plotly_white",
               xaxis = list(title = "log return"),
               yaxis = list(title = "count"))
    })
    
    output$dow_ret = renderPlotly({
      df = sampled()
      req(!is.null(df))
      if (!all(c("date", "ret_1") %in% names(df))) return(NULL)
      
      tmp = df |>
        mutate(dow = wday(as.Date(date), label = TRUE, week_start = 1)) |>
        group_by(dow) |>
        summarise(mean_ret = mean(ret_1, na.rm = TRUE), n = dplyr::n(), .groups = "drop")
      
      plot_ly(tmp, x = ~dow, y = ~mean_ret, type = "bar",
              hovertemplate = "DOW: %{x}<br>mean ret: %{y:.6f}<br>n: %{customdata}<extra></extra>",
              customdata = ~n) |>
        layout(template = "plotly_white",
               xaxis = list(title = ""),
               yaxis = list(title = "mean log return"))
    })
    
    output$vol_regime = renderPlotly({
      df = sampled()
      req(!is.null(df))
      if (!"roll_sd_60" %in% names(df)) return(NULL)
      
      v = df$roll_sd_60
      v = v[is.finite(v)]
      if (length(v) < 80) return(NULL)
      
      qs = quantile(v, probs = c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
      tmp = df |>
        mutate(
          vol = roll_sd_60,
          regime = case_when(
            vol <= qs[[1]] ~ "Q1 (low)",
            vol <= qs[[2]] ~ "Q2",
            vol <= qs[[3]] ~ "Q3",
            vol <= qs[[4]] ~ "Q4",
            TRUE ~ "Q5 (high)"
          )
        ) |>
        count(regime) |>
        mutate(regime = factor(regime, levels = c("Q1 (low)", "Q2", "Q3", "Q4", "Q5 (high)"))) |>
        arrange(regime)
      
      plot_ly(tmp, x = ~regime, y = ~n, type = "bar",
              hovertemplate = "Regime: %{x}<br>count: %{y}<extra></extra>") |>
        layout(template = "plotly_white",
               xaxis = list(title = ""),
               yaxis = list(title = "count"))
    })
    
    output$acf_panel = renderPlotly({
      df = sampled()
      req(!is.null(df))
      if (!"ret_1" %in% names(df)) return(NULL)
      
      r = df$ret_1
      r = r[is.finite(r)]
      if (length(r) < 80) return(NULL)
      
      lag_max = min(80, length(r) - 2)
      
      acf_r = stats::acf(r, plot = FALSE, lag.max = lag_max)
      acf_sq = stats::acf(r^2, plot = FALSE, lag.max = lag_max)
      
      d1 = data.frame(lag = as.numeric(acf_r$lag), acf = as.numeric(acf_r$acf), series = "ret")
      d2 = data.frame(lag = as.numeric(acf_sq$lag), acf = as.numeric(acf_sq$acf), series = "ret^2")
      dd = bind_rows(d1, d2) |> filter(lag > 0)
      
      plot_ly(dd, x = ~lag, y = ~acf, color = ~series, type = "scatter", mode = "lines",
              hovertemplate = "lag: %{x}<br>acf: %{y:.3f}<extra></extra>") |>
        layout(template = "plotly_white", xaxis = list(title = "lag"), yaxis = list(title = "ACF"))
    })
  })
}

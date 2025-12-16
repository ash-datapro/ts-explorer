# plots.R — Plot helpers (light theme) for AMZN time-series dashboard

suppressPackageStartupMessages({
  library(plotly)
  library(dplyr)
  library(scales)
  library(stats)
})

# Common plot styling -----------------------------------------------------------
apply_light = function(p, title = NULL, y_tickprefix = NULL, y_tickformat = NULL) {
  if (is.null(p)) return(NULL)
  
  p |>
    layout(
      template = "plotly_white",
      paper_bgcolor = "#ffffff",
      plot_bgcolor  = "#ffffff",
      font = list(color = "#111111"),
      title = if (!is.null(title)) list(text = title, x = 0, xanchor = "left") else NULL,
      margin = list(l = 60, r = 20, t = if (!is.null(title)) 55 else 25, b = 50),
      xaxis = list(
        title = "",
        gridcolor = "#e5e7eb",
        zerolinecolor = "#e5e7eb",
        automargin = TRUE
      ),
      yaxis = list(
        title = "",
        gridcolor = "#e5e7eb",
        zerolinecolor = "#e5e7eb",
        automargin = TRUE,
        tickprefix = y_tickprefix,
        tickformat = y_tickformat
      )
    )
}

# Price ------------------------------------------------------------------------
plot_price_ts = function(df, log_scale = FALSE) {
  df = df |> arrange(date)
  if (!all(c("date", "amzn") %in% names(df))) return(NULL)
  
  y = if (log_scale) log(df$amzn) else df$amzn
  ylab = if (log_scale) "log(price)" else "price"
  
  p = plot_ly(
    df,
    x = ~date,
    y = y,
    type = "scatter",
    mode = "lines",
    name = "AMZN",
    hovertemplate = if (log_scale) {
      "Date: %{x}<br>log(price): %{y:.4f}<extra></extra>"
    } else {
      "Date: %{x}<br>price: $%{y:,.2f}<extra></extra>"
    }
  ) |>
    layout(
      yaxis = list(title = ylab)
    )
  
  apply_light(
    p,
    title = if (log_scale) "AMZN log price" else "AMZN price",
    y_tickprefix = if (!log_scale) "$" else NULL
  )
}

# Returns ----------------------------------------------------------------------
plot_returns_ts = function(df) {
  if (!"ret_1" %in% names(df)) return(NULL)
  
  d = df |> filter(is.finite(ret_1)) |> arrange(date)
  if (nrow(d) < 5) return(NULL)
  
  p = plot_ly(
    d,
    x = ~date,
    y = ~ret_1,
    type = "scatter",
    mode = "lines",
    name = "log return",
    hovertemplate = "Date: %{x}<br>log return: %{y:.6f}<extra></extra>"
  ) |>
    layout(
      yaxis = list(title = "log return")
    )
  
  apply_light(p, title = "Daily log returns")
}

# Rolling volatility -----------------------------------------------------------
plot_roll_vol_ts = function(df, window = 60) {
  col = if (window == 20) "roll_sd_20" else "roll_sd_60"
  if (!col %in% names(df)) return(NULL)
  
  d = df |> filter(is.finite(.data[[col]])) |> arrange(date)
  if (nrow(d) < 5) return(NULL)
  
  p = plot_ly(
    d,
    x = ~date,
    y = d[[col]],
    type = "scatter",
    mode = "lines",
    name = paste0("sd(", window, ")"),
    hovertemplate = paste0("Date: %{x}<br>sd(", window, "): %{y:.6f}<extra></extra>")
  ) |>
    layout(
      yaxis = list(title = "sd(log return)")
    )
  
  apply_light(p, title = paste0("Rolling volatility (", window, "d SD of returns)"))
}

# Drawdown ---------------------------------------------------------------------
plot_drawdown_ts = function(df) {
  if (!"drawdown" %in% names(df)) return(NULL)
  
  d = df |> arrange(date)
  if (nrow(d) < 5) return(NULL)
  
  p = plot_ly(
    d,
    x = ~date,
    y = ~drawdown,
    type = "scatter",
    mode = "lines",
    name = "drawdown",
    hovertemplate = "Date: %{x}<br>drawdown: %{y:.2%}<extra></extra>"
  ) |>
    layout(
      yaxis = list(title = "drawdown")
    )
  
  apply_light(p, title = "Drawdown", y_tickformat = ".0%")
}

# ACF --------------------------------------------------------------------------
plot_acf_ts = function(x, lag_max = 80, title = "ACF") {
  x = x[is.finite(x)]
  if (length(x) < 80) return(NULL)
  
  lag_max = min(lag_max, length(x) - 2)
  
  a = stats::acf(x, plot = FALSE, lag.max = lag_max)
  d = data.frame(lag = as.numeric(a$lag), acf = as.numeric(a$acf)) |>
    dplyr::filter(lag > 0)
  
  p = plot_ly(
    d,
    x = ~lag,
    y = ~acf,
    type = "scatter",
    mode = "lines",
    name = "acf",
    hovertemplate = "lag: %{x}<br>acf: %{y:.3f}<extra></extra>"
  ) |>
    layout(
      yaxis = list(title = "acf"),
      xaxis = list(title = "lag")
    )
  
  apply_light(p, title = title)
}

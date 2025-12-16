# metrics.R — KPI tiles module (time-series)

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(scales)
})

metrics_ui = function(id) {
  ns = NS(id)
  
  fluidRow(
    column(3, uiOutput(ns("kpi_price"))),
    column(3, uiOutput(ns("kpi_ret"))),
    column(3, uiOutput(ns("kpi_vol"))),
    column(3, uiOutput(ns("kpi_dd")))
  )
}

metrics_server = function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    value_box = function(title, value, subtitle = NULL, badge = NULL) {
      tags$div(
        class = "value-box",
        tags$div(
          class = "vb-title",
          tags$span(title),
          if (!is.null(badge)) tags$span(badge, style = "float:right; font-weight:600;")
        ),
        tags$div(value, class = "vb-value"),
        if (!is.null(subtitle)) tags$div(subtitle, class = "vb-sub")
      )
    }
    
    fmt_pct = function(x, digits = 2) {
      if (!is.finite(x)) return("—")
      paste0(sprintf(paste0("%.", digits, "f"), x), "%")
    }
    
    kpis = reactive({
      df = data()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      
      df = df |> arrange(date)
      last_i = nrow(df)
      
      last_price = suppressWarnings(as.numeric(df$amzn[[last_i]]))
      last_date  = df$date[[last_i]]
      
      # last daily return (log)
      last_ret = if ("ret_1" %in% names(df)) suppressWarnings(as.numeric(df$ret_1[[last_i]])) else NA_real_
      last_ret_pct = if (is.finite(last_ret)) (exp(last_ret) - 1) * 100 else NA_real_
      
      # 20d ann vol
      vol20 = if ("roll_sd_20" %in% names(df)) suppressWarnings(as.numeric(df$roll_sd_20[[last_i]])) else NA_real_
      vol20_ann = if (is.finite(vol20)) vol20 * sqrt(252) * 100 else NA_real_
      
      # max drawdown over window
      dd = if ("drawdown" %in% names(df)) suppressWarnings(min(df$drawdown, na.rm = TRUE)) else NA_real_
      dd_pct = if (is.finite(dd)) dd * 100 else NA_real_
      
      list(
        last_price = last_price,
        last_date = last_date,
        last_ret_pct = last_ret_pct,
        vol20_ann = vol20_ann,
        max_drawdown_pct = dd_pct
      )
    })
    
    output$kpi_price = renderUI({
      k = kpis()
      if (is.null(k)) return(NULL)
      
      val = if (is.finite(k$last_price)) scales::dollar(k$last_price) else "—"
      sub = if (!is.null(k$last_date) && !is.na(k$last_date)) paste0("as of ", k$last_date) else NULL
      
      value_box("Last Price", val, sub)
    })
    
    output$kpi_ret = renderUI({
      k = kpis()
      if (is.null(k)) return(NULL)
      
      badge = NULL
      if (is.finite(k$last_ret_pct)) {
        badge = if (k$last_ret_pct >= 0) "▲" else "▼"
      }
      
      value_box(
        "Last Daily Return",
        fmt_pct(k$last_ret_pct, digits = 2),
        "exp(log-return) - 1",
        badge = badge
      )
    })
    
    output$kpi_vol = renderUI({
      k = kpis()
      if (is.null(k)) return(NULL)
      
      value_box(
        "20d Ann. Vol",
        fmt_pct(k$vol20_ann, digits = 1),
        "sd(ret) × sqrt(252)"
      )
    })
    
    output$kpi_dd = renderUI({
      k = kpis()
      if (is.null(k)) return(NULL)
      
      value_box(
        "Max Drawdown",
        fmt_pct(k$max_drawdown_pct, digits = 1),
        "min(price / peak - 1)"
      )
    })
  })
}

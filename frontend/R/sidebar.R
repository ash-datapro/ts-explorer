# sidebar.R — Sidebar module (time-series filters + forecast controls)
# Returns: filtered() + controls() for app.R.

suppressPackageStartupMessages({
  library(shiny)
  library(lubridate)
})

sidebar_ui = function(id) {
  ns = NS(id)
  
  tagList(
    h3("Controls"),
    p(class = "text-muted", "Filter the visible window and configure forecast inputs."),
    
    tags$div(
      style = "margin-top: 6px;",
      tags$div(style = "font-weight: 700; margin-bottom: 6px;", "Data window"),
      
      dateRangeInput(
        ns("date_range"), "Date range",
        start = Sys.Date() - 365 * 3, end = Sys.Date(),
        format = "yyyy-mm-dd"
      ),
      
      selectInput(
        ns("view_mode"), "Series view",
        choices = c("Price" = "price", "Log Price" = "log"),
        selected = "price"
      ),
      
      selectInput(
        ns("vol_window"), "Volatility window",
        choices = c("20 trading days" = 20, "60 trading days" = 60),
        selected = 60
      ),
      
      checkboxInput(ns("show_returns"), "Show returns panel", value = TRUE)
    ),
    
    hr(),
    
    tags$div(
      tags$div(style = "font-weight: 700; margin-bottom: 6px;", "Forecast request"),
      
      dateInput(ns("as_of"), "As-of date", value = Sys.Date(), format = "yyyy-mm-dd"),
      selectInput(ns("horizon"), "Horizon (days)", choices = c("1" = 1, "5" = 5, "20" = 20), selected = 5),
      checkboxInput(ns("predict_all"), "Return all horizons (1/5/20)", value = FALSE)
    ),
    
    hr(),
    
    fluidRow(
      column(6, actionButton(ns("refresh_btn"), "Refresh", class = "btn-default")),
      column(6, div(style = "text-align:right;", actionButton(ns("reset"), "Reset", class = "btn-default")))
    ),
    
    tags$div(
      style = "margin-top:10px;",
      tags$small(class = "text-muted", textOutput(ns("rows_badge"), inline = TRUE))
    )
  )
}

sidebar_server = function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Keep date controls aligned with data range
    observeEvent(data(), {
      df = data()
      if (is.null(df) || nrow(df) == 0) return()
      
      d = as.Date(df$date)
      dmin = suppressWarnings(min(d, na.rm = TRUE))
      dmax = suppressWarnings(max(d, na.rm = TRUE))
      
      if (is.finite(dmin) && is.finite(dmax)) {
        updateDateRangeInput(session, "date_range", start = dmin, end = dmax)
        updateDateInput(session, "as_of", value = dmax)
      }
    }, ignoreInit = FALSE)
    
    observeEvent(input$reset, {
      updateSelectInput(session, "view_mode", selected = "price")
      updateSelectInput(session, "vol_window", selected = 60)
      updateCheckboxInput(session, "show_returns", value = TRUE)
      updateCheckboxInput(session, "predict_all", value = FALSE)
      updateSelectInput(session, "horizon", selected = 5)
      
      df = data()
      if (!is.null(df) && nrow(df) > 0) {
        d = as.Date(df$date)
        dmin = suppressWarnings(min(d, na.rm = TRUE))
        dmax = suppressWarnings(max(d, na.rm = TRUE))
        if (is.finite(dmin) && is.finite(dmax)) {
          updateDateRangeInput(session, "date_range", start = dmin, end = dmax)
          updateDateInput(session, "as_of", value = dmax)
        }
      }
    })
    
    # Filtered data (date range only)
    filtered = reactive({
      df = data()
      if (is.null(df) || nrow(df) == 0) return(df)
      
      dr = input$date_range
      if (!is.null(dr) && length(dr) == 2 && all(!is.na(dr))) {
        df = df[df$date >= as.Date(dr[[1]]) & df$date <= as.Date(dr[[2]]), , drop = FALSE]
      }
      df
    })
    
    output$rows_badge = renderText({
      n = tryCatch(nrow(filtered()), error = function(e) 0)
      paste0("Rows: ", format(n, big.mark = ","))
    })
    
    list(
      filtered = filtered,
      controls = reactive(list(
        date_range = input$date_range,
        view_mode = input$view_mode,
        vol_window = as.integer(input$vol_window),
        show_returns = isTRUE(input$show_returns),
        
        # Forecast inputs (used by forecast module/tab)
        as_of = as.Date(input$as_of),
        horizon = as.integer(input$horizon),
        predict_all = isTRUE(input$predict_all),
        
        # Click signal for refresh
        refresh_click = input$refresh_btn
      ))
    )
  })
}

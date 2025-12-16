# utils.R — shared helpers for AMZN time-series Shiny + API
# (DB connection, data loader, feature helpers, API client, formatting)

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(lubridate)
  library(slider)
  library(httr)
  library(jsonlite)
  library(scales)
})

# ---------------------------
# Env config
# ---------------------------
DB_USER = Sys.getenv("DB_USER", "amzn_user")
DB_PASSWORD = Sys.getenv("DB_PASSWORD", "")
DB_HOST = Sys.getenv("DB_HOST", "localhost")
DB_PORT = as.integer(Sys.getenv("DB_PORT", "5432"))
DB_NAME = Sys.getenv("DB_NAME", "amzn_ts")

AMZN_SCHEMA = Sys.getenv("AMZN_SCHEMA", "finance")
AMZN_TABLE = Sys.getenv("AMZN_TABLE", "amzn_daily")

PREDICT_API_URL = Sys.getenv("PREDICT_API_URL", "http://127.0.0.1:8000/predict")

# ---------------------------
# Small utilities
# ---------------------------
validate_cols = function(df, cols) {
  missing = setdiff(cols, names(df))
  if (length(missing) > 0) stop("Missing required columns: ", paste(missing, collapse = ", "))
  invisible(TRUE)
}

fmt_pct = function(x, digits = 2) {
  if (!is.finite(x)) return("—")
  paste0(sprintf(paste0("%.", digits, "f"), x), "%")
}

fmt_num = function(x, digits = 4) {
  if (!is.finite(x)) return("—")
  sprintf(paste0("%.", digits, "f"), x)
}

# ---------------------------
# Postgres helpers
# ---------------------------
get_db_connection = function() {
  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = DB_NAME,
    host = DB_HOST,
    port = DB_PORT,
    user = DB_USER,
    password = DB_PASSWORD
  )
}

load_data = function(limit = NULL) {
  # Returns: date, amzn
  con = get_db_connection()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  fqn = paste0(AMZN_SCHEMA, ".", AMZN_TABLE)
  
  q = paste0("SELECT date, amzn FROM ", fqn, " ORDER BY date;")
  if (!is.null(limit)) {
    q = paste0("SELECT date, amzn FROM ", fqn, " ORDER BY date DESC LIMIT ", as.integer(limit), ";")
  }
  
  df = DBI::dbGetQuery(con, q)
  
  df = df |>
    mutate(
      date = as.Date(date),
      amzn = as.numeric(amzn)
    ) |>
    filter(!is.na(date), !is.na(amzn)) |>
    arrange(date)
  
  if (any(duplicated(df$date))) {
    df = df |>
      group_by(date) |>
      summarise(amzn = last(amzn), .groups = "drop") |>
      arrange(date)
  }
  
  df
}

latest_db_date = function() {
  con = get_db_connection()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  fqn = paste0(AMZN_SCHEMA, ".", AMZN_TABLE)
  d = DBI::dbGetQuery(con, paste0("SELECT MAX(date) AS max_date FROM ", fqn, ";"))
  as.Date(d$max_date[[1]])
}

# ---------------------------
# Feature engineering (EDA visuals, leakage-safe)
# ---------------------------
compute_features = function(df) {
  validate_cols(df, c("date", "amzn"))
  
  df = df |>
    arrange(date) |>
    mutate(
      log_price = log(amzn),
      ret_1 = log_price - lag(log_price),
      roll_sd_20 = slider::slide_dbl(ret_1, sd, .before = 19, .complete = TRUE, na.rm = TRUE),
      roll_sd_60 = slider::slide_dbl(ret_1, sd, .before = 59, .complete = TRUE, na.rm = TRUE),
      peak = cummax(amzn),
      drawdown = amzn / peak - 1
    )
  
  df
}

# ---------------------------
# API client helpers (Plumber)
# ---------------------------
api_health = function() {
  health = sub("/predict$", "/health", PREDICT_API_URL)
  res = httr::GET(health, httr::timeout(2))
  list(status_code = httr::status_code(res), ok = httr::status_code(res) == 200)
}

api_meta = function() {
  meta = sub("/predict$", "/meta", PREDICT_API_URL)
  res = httr::GET(meta, httr::timeout(3))
  if (httr::http_error(res)) return(NULL)
  httr::content(res, as = "parsed", simplifyVector = TRUE)
}

api_predict = function(as_of, horizon = NULL) {
  body = list(as_of = as.character(as.Date(as_of)))
  if (!is.null(horizon)) body$horizon = as.integer(horizon)
  
  res = httr::POST(
    url = PREDICT_API_URL,
    encode = "json",
    body = body,
    httr::timeout(10)
  )
  
  if (httr::http_error(res)) {
    txt = httr::content(res, as = "text", encoding = "UTF-8")
    stop("API error (", httr::status_code(res), "): ", txt)
  }
  
  httr::content(res, as = "parsed", simplifyVector = TRUE)
}

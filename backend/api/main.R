# main.R — AMZN Time Series Forecast API (plumber)
# Uses model.rds (bundle with fitted workflows per horizon)
# Reads prices from Postgres: finance.amzn_daily(date, amzn)

suppressPackageStartupMessages({
  library(plumber)
  library(jsonlite)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(lubridate)
  library(slider)
  library(workflows)
  library(parsnip)
})

# ---------------------------
# 0) Load model bundle
# ---------------------------
model_bundle = NULL
tryCatch({
  model_bundle = readRDS("model.rds")
}, error = function(e) {
  model_bundle <<- NULL
  message("Error loading model.rds: ", e$message)
})

#* @plumber
function(pr) {
  pr$setErrorHandler(function(req, res, err) {
    msg = conditionMessage(err)
    
    # JSON parse errors happen before your endpoint runs; treat them as 400
    if (grepl("lexical error|invalid char in json text|parse", msg, ignore.case = TRUE)) {
      res$status = 400
      return(list(detail = paste0("Invalid JSON body: ", msg)))
    }
    
    # Everything else
    res$status = 500
    return(list(detail = msg))
  })
  
  pr
}

# Expected horizons and fitted models
allowed_h = c(1L, 5L, 20L)

get_fitted_for_h = function(h) {
  if (is.null(model_bundle)) stop("Model bundle not loaded")
  nm = paste0("h", as.integer(h))
  fitted = model_bundle$fitted_by_h[[nm]]
  if (is.null(fitted)) stop("No fitted model found for horizon = ", h)
  fitted
}

# ---------------------------
# 1) DB connection (env-driven)
# ---------------------------
db_user = Sys.getenv("DB_USER", unset = "amzn_user")
db_password = Sys.getenv("DB_PASSWORD", unset = "")
db_host = Sys.getenv("DB_HOST", unset = "localhost")
db_port = as.integer(Sys.getenv("DB_PORT", unset = "5432"))
db_name = Sys.getenv("DB_NAME", unset = "amzn_ts")

db_schema = Sys.getenv("AMZN_SCHEMA", unset = "finance")
db_table  = Sys.getenv("AMZN_TABLE",  unset = "amzn_daily")

db_table_fqn = paste0(db_schema, ".", db_table)

connect_db = function() {
  con = dbConnect(
    RPostgres::Postgres(),
    dbname = db_name,
    host = db_host,
    port = db_port,
    user = db_user,
    password = db_password
  )
  con
}

fetch_prices = function(con, end_date, limit_n = 260L) {
  # Pull last N rows up to end_date (enough for lags + 60d rolls)
  q = paste0(
    "SELECT date, amzn FROM ", db_table_fqn,
    " WHERE date <= $1 ORDER BY date DESC LIMIT $2;"
  )
  
  r = dbSendQuery(con, q)
  on.exit(dbClearResult(r), add = TRUE)
  dbBind(r, list(as.Date(end_date), as.integer(limit_n)))
  df = dbFetch(r)
  
  df = df |>
    mutate(date = as.Date(date), amzn = as.numeric(amzn)) |>
    filter(!is.na(date), !is.na(amzn)) |>
    arrange(date)
  
  if (nrow(df) == 0) stop("No rows returned from ", db_table_fqn, " for end_date = ", end_date)
  
  if (any(duplicated(df$date))) {
    df = df |>
      group_by(date) |>
      summarise(amzn = last(amzn), .groups = "drop") |>
      arrange(date)
  }
  
  df
}

# ---------------------------
# 2) Feature engineering (must match training)
# ---------------------------
engineer_features = function(df) {
  df = df |>
    arrange(date) |>
    mutate(
      log_price = log(amzn),
      ret_1 = log_price - dplyr::lag(log_price)
    ) |>
    mutate(
      lag_ret_1  = dplyr::lag(ret_1, 1),
      lag_ret_2  = dplyr::lag(ret_1, 2),
      lag_ret_3  = dplyr::lag(ret_1, 3),
      lag_ret_5  = dplyr::lag(ret_1, 5),
      lag_ret_10 = dplyr::lag(ret_1, 10),
      lag_ret_20 = dplyr::lag(ret_1, 20),
      
      roll_mean_5  = slider::slide_dbl(ret_1, mean, .before = 5,  .complete = TRUE, na.rm = TRUE),
      roll_mean_20 = slider::slide_dbl(ret_1, mean, .before = 20, .complete = TRUE, na.rm = TRUE),
      
      roll_sd_5  = slider::slide_dbl(ret_1, sd, .before = 5,  .complete = TRUE, na.rm = TRUE),
      roll_sd_20 = slider::slide_dbl(ret_1, sd, .before = 20, .complete = TRUE, na.rm = TRUE),
      roll_sd_60 = slider::slide_dbl(ret_1, sd, .before = 60, .complete = TRUE, na.rm = TRUE),
      
      mom_5  = slider::slide_dbl(ret_1, sum, .before = 5,  .complete = TRUE, na.rm = TRUE),
      mom_20 = slider::slide_dbl(ret_1, sum, .before = 20, .complete = TRUE, na.rm = TRUE),
      mom_60 = slider::slide_dbl(ret_1, sum, .before = 60, .complete = TRUE, na.rm = TRUE),
      
      dow = lubridate::wday(date, label = TRUE, week_start = 1),
      month = lubridate::month(date, label = TRUE)
    )
  
  df
}

make_feature_row = function(feat_df, as_of) {
  row = feat_df |>
    filter(date <= as.Date(as_of)) |>
    slice_tail(n = 1) |>
    select(
      date,
      starts_with("lag_ret_"),
      starts_with("roll_mean_"),
      starts_with("roll_sd_"),
      starts_with("mom_"),
      dow, month
    )
  
  if (nrow(row) != 1) stop("Could not isolate a single row for as_of = ", as_of)
  
  # Stabilize factor levels (avoid ordered-factor surprises)
  row = row |>
    mutate(
      dow = factor(
        as.character(dow),
        levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),
        ordered = TRUE
      ),
      month = factor(
        as.character(month),
        levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
        ordered = TRUE
      )
    )
  
  # Ensure we have enough history to compute features (no NA in numeric predictors)
  bad = vapply(row |> select(-date, -dow, -month), function(x) any(is.na(x)), logical(1))
  if (any(bad)) {
    miss = paste(names(bad)[bad], collapse = ", ")
    stop("Insufficient history to compute features at as_of=", as_of, ". Missing: ", miss)
  }
  
  row
}

predict_return = function(h, feature_row) {
  fitted = get_fitted_for_h(h)
  pred = tryCatch(
    predict(fitted, new_data = feature_row, type = "numeric"),
    error = function(e) e
  )
  if (inherits(pred, "error")) stop(pred$message)
  as.numeric(pred$.pred[[1]])
}

# ---------------------------
# 3) Input validation
# ---------------------------
validate_body = function(body) {
  out = list(
    horizon = body[["horizon"]],
    as_of = body[["as_of"]]
  )
  
  # horizon can be NULL (meaning "all") or numeric (single)
  if (!is.null(out$horizon)) {
    out$horizon = as.integer(out$horizon)
    if (!(out$horizon %in% allowed_h)) stop("horizon must be one of: ", paste(allowed_h, collapse = ", "))
  }
  
  # as_of can be NULL => use latest available date
  if (!is.null(out$as_of)) {
    out$as_of = as.Date(as.character(out$as_of))
    if (is.na(out$as_of)) stop("as_of must be a valid date (YYYY-MM-DD)")
  }
  
  out
}

latest_date = function(con) {
  q = paste0("SELECT MAX(date) AS max_date FROM ", db_table_fqn, ";")
  d = dbGetQuery(con, q)
  as.Date(d$max_date[[1]])
}

#* Health check
#* @get /health
function(res) {
  list(
    status = "ok",
    model_loaded = !is.null(model_bundle),
    horizons = allowed_h,
    db = list(host = db_host, port = db_port, dbname = db_name, table = db_table_fqn)
  )
}

#* Model metadata
#* @get /meta
function(res) {
  if (is.null(model_bundle)) {
    res$status = 503
    return(list(detail = "Model not loaded"))
  }
  
  list(
    created_utc = model_bundle$created_utc,
    target = model_bundle$target,
    horizons = model_bundle$horizons,
    data_source = model_bundle$data_source,
    cv = model_bundle$cv,
    champions = model_bundle$champions
  )
}

#* Predict h-day-ahead daily log return (direct)
#* @post /predict
#* @parser json
#* @serializer json
function(req, res) {
  if (is.null(model_bundle)) {
    res$status = 503
    return(list(detail = "Model not loaded"))
  }
  
  payload = req$body
  if (is.null(payload)) payload = list()
  if (!is.list(payload)) {
    res$status = 400
    return(list(detail = "JSON body must be an object"))
  }
  
  args = tryCatch(validate_body(payload), error = function(e) e)
  if (inherits(args, "error")) {
    res$status = 400
    return(list(detail = args$message))
  }
  
  con = NULL
  con = tryCatch(connect_db(), error = function(e) e)
  if (inherits(con, "error")) {
    res$status = 503
    return(list(detail = paste0("DB connection error: ", con$message)))
  }
  on.exit({ if (!is.null(con) && dbIsValid(con)) dbDisconnect(con) }, add = TRUE)
  
  as_of = args$as_of
  if (is.null(as_of)) {
    as_of = tryCatch(latest_date(con), error = function(e) e)
    if (inherits(as_of, "error") || is.na(as_of)) {
      res$status = 500
      return(list(detail = "Could not determine latest date from database"))
    }
  }
  
  # Pull history and compute features
  df = tryCatch(fetch_prices(con, end_date = as_of, limit_n = 260L), error = function(e) e)
  if (inherits(df, "error")) {
    res$status = 500
    return(list(detail = paste0("Data fetch error: ", df$message)))
  }
  
  feat = tryCatch(engineer_features(df), error = function(e) e)
  if (inherits(feat, "error")) {
    res$status = 500
    return(list(detail = paste0("Feature engineering error: ", feat$message)))
  }
  
  xrow = tryCatch(make_feature_row(feat, as_of = as_of), error = function(e) e)
  if (inherits(xrow, "error")) {
    res$status = 400
    return(list(detail = paste0("Feature row error: ", xrow$message)))
  }
  
  # Predict (single horizon or all)
  hs = if (is.null(args$horizon)) allowed_h else as.integer(args$horizon)
  
  preds = list()
  for (h in hs) {
    ph = tryCatch(predict_return(h, xrow), error = function(e) e)
    if (inherits(ph, "error")) {
      res$status = 500
      return(list(detail = paste0("Prediction error (h=", h, "): ", ph$message)))
    }
    preds[[paste0("h", h)]] = jsonlite::unbox(ph)
  }
  
  # Include current price for context (not a price forecast)
  cur_price = df$amzn[[nrow(df)]]
  
  list(
    as_of = as.character(as_of),
    current_price = jsonlite::unbox(as.numeric(cur_price)),
    prediction = preds,
    interpretation = "Each prediction is the model's estimate of the single-day log return occurring at horizon h (direct)."
  )
}

# ============================================================
# Time-series load + EDA for
# File schema detected: date (YYYY-MM-DD), AMZN (numeric)
# ============================================================

options(stringsAsFactors = FALSE, scipen = 999)
set.seed(42)

# ---------------------------
# 0) Packages (auto-install)
# ---------------------------
pkgs = c(
  "data.table", "readr", "dplyr", "tidyr", "lubridate", "ggplot2",
  "scales", "tsibble", "feasts", "fabletools", "forecast",
  "tseries", "urca", "zoo", "slider", "strucchange", "changepoint",
  "FinTS", "broom", "patchwork"
)

ip = rownames(installed.packages())
to_install = setdiff(pkgs, ip)
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

invisible(lapply(pkgs, library, character.only = TRUE))

# ---------------------------
# 1) Paths + I/O
# ---------------------------
data_path = "~/Desktop/Project/time-series/data/amazon_stock.csv"   # change if needed
out_dir = "~/Desktop/Project/time-series/data/"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

setwd(out_dir)

# Robust read (handles big files, but also fine for small)
raw = data.table::fread(data_path, na.strings = c("", "NA", "NaN", "null", "NULL"))

# ---------------------------
# 2) Schema + type enforcement
# ---------------------------
stopifnot(all(c("date", "AMZN") %in% names(raw)))

dt = raw |>
  dplyr::transmute(
    date = lubridate::ymd(date, quiet = TRUE),
    AMZN = as.numeric(AMZN)
  ) |>
  dplyr::filter(!is.na(date)) |>
  dplyr::arrange(date)

# Duplicate detection (keep the last observation if duplicates exist)
dup_n = sum(duplicated(dt$date))
if (dup_n > 0) {
  message("Found duplicates on date = ", dup_n, " rows. Keeping last per date.")
  dt = dt |>
    dplyr::group_by(date) |>
    dplyr::summarise(AMZN = dplyr::last(AMZN), .groups = "drop") |>
    dplyr::arrange(date)
}

# Missing value diagnostics
na_n = sum(is.na(dt$AMZN))
message("Rows = ", nrow(dt), " | NA(AMZN) = ", na_n)

# Basic invariants
stopifnot(isTRUE(all(diff(dt$date) >= 0)))

# ---------------------------
# 3) Calendar completeness checks
# ---------------------------
min_d = min(dt$date, na.rm = TRUE)
max_d = max(dt$date, na.rm = TRUE)

full_cal = tibble::tibble(date = seq.Date(min_d, max_d, by = "day"))

# Missing calendar days (includes weekends)
missing_any = dplyr::anti_join(full_cal, dt |> dplyr::select(date), by = "date")

# Missing business days (Mon-Fri only; excludes weekends)
biz_cal = full_cal |>
  dplyr::mutate(wday = lubridate::wday(date, week_start = 1)) |>
  dplyr::filter(wday <= 5) |>
  dplyr::select(date)

missing_biz = dplyr::anti_join(biz_cal, dt |> dplyr::select(date), by = "date")

data.table::fwrite(missing_any, file.path(out_dir, "missing_calendar_days.csv"))
data.table::fwrite(missing_biz, file.path(out_dir, "missing_business_days.csv"))

message("Missing calendar days = ", nrow(missing_any),
        " | Missing business days = ", nrow(missing_biz))

# Create a complete daily panel (keeps NA on missing dates; no imputation by default)
dt_full = full_cal |>
  dplyr::left_join(dt, by = "date") |>
  dplyr::arrange(date)

# Optional: impute missing AMZN (LOCF then NOCB) for methods requiring no NA
dt_full = dt_full |>
  dplyr::mutate(
    AMZN_locf = zoo::na.locf(AMZN, na.rm = FALSE),
    AMZN_locf = zoo::na.locf(AMZN_locf, fromLast = TRUE, na.rm = FALSE)
  )

# ---------------------------
# 4) Build tsibble objects
# ---------------------------
ts_price = dt |>
  tsibble::as_tsibble(index = date)

ts_price_full = dt_full |>
  dplyr::select(date, AMZN = AMZN_locf) |>
  tsibble::as_tsibble(index = date)

# ---------------------------
# 5) Feature engineering
# ---------------------------
feat = ts_price_full |>
  dplyr::mutate(
    log_price = log(AMZN),
    # log returns (daily)
    ret = log_price - dplyr::lag(log_price),
    abs_ret = abs(ret),
    sq_ret = ret^2,
    # calendar features
    dow = lubridate::wday(date, label = TRUE, week_start = 1),
    month = lubridate::month(date, label = TRUE),
    year = lubridate::year(date)
  )

# Rolling stats (choose windows typical for daily financial series)
feat = feat |>
  dplyr::mutate(
    roll_mean_20 = slider::slide_dbl(ret, mean, .before = 19, .complete = TRUE, na.rm = TRUE),
    roll_sd_20 = slider::slide_dbl(ret, sd, .before = 19, .complete = TRUE, na.rm = TRUE),
    roll_sd_60 = slider::slide_dbl(ret, sd, .before = 59, .complete = TRUE, na.rm = TRUE),
    roll_skew_60 = slider::slide_dbl(ret, \(x) mean(((x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE))^3, na.rm = TRUE),
                                     .before = 59, .complete = TRUE),
    roll_kurt_60 = slider::slide_dbl(ret, \(x) mean(((x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE))^4, na.rm = TRUE),
                                     .before = 59, .complete = TRUE)
  )

# ---------------------------
# 6) Core summaries (exportable)
# ---------------------------
summ_price = dt |>
  dplyr::summarise(
    start = min(date), end = max(date),
    n = dplyr::n(),
    min = min(AMZN, na.rm = TRUE),
    p01 = quantile(AMZN, 0.01, na.rm = TRUE),
    p05 = quantile(AMZN, 0.05, na.rm = TRUE),
    median = median(AMZN, na.rm = TRUE),
    mean = mean(AMZN, na.rm = TRUE),
    p95 = quantile(AMZN, 0.95, na.rm = TRUE),
    p99 = quantile(AMZN, 0.99, na.rm = TRUE),
    max = max(AMZN, na.rm = TRUE),
    sd = sd(AMZN, na.rm = TRUE),
    na = sum(is.na(AMZN))
  )

summ_ret = feat |>
  dplyr::filter(!is.na(ret)) |>
  dplyr::summarise(
    n = dplyr::n(),
    mean = mean(ret, na.rm = TRUE),
    sd = sd(ret, na.rm = TRUE),
    skew = mean(((ret - mean(ret))/sd(ret))^3, na.rm = TRUE),
    kurt = mean(((ret - mean(ret))/sd(ret))^4, na.rm = TRUE),
    min = min(ret, na.rm = TRUE),
    p01 = quantile(ret, 0.01, na.rm = TRUE),
    p05 = quantile(ret, 0.05, na.rm = TRUE),
    median = median(ret, na.rm = TRUE),
    p95 = quantile(ret, 0.95, na.rm = TRUE),
    p99 = quantile(ret, 0.99, na.rm = TRUE),
    max = max(ret, na.rm = TRUE)
  )

data.table::fwrite(summ_price, file.path(out_dir, "summary_price.csv"))
data.table::fwrite(summ_ret, file.path(out_dir, "summary_returns.csv"))

# ---------------------------
# 7) Plots (save to disk)
# ---------------------------
p_price = ggplot(feat, aes(x = date, y = AMZN)) +
  geom_line(linewidth = 0.35) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "AMZN price level", x = NULL, y = "Price") +
  theme_minimal(base_size = 12)

p_log = ggplot(feat, aes(x = date, y = log_price)) +
  geom_line(linewidth = 0.35) +
  labs(title = "Log price", x = NULL, y = "log(Price)") +
  theme_minimal(base_size = 12)

p_ret = ggplot(feat |> dplyr::filter(!is.na(ret)), aes(x = date, y = ret)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_line(linewidth = 0.3) +
  labs(title = "Daily log returns", x = NULL, y = "Return") +
  theme_minimal(base_size = 12)

p_vol = ggplot(feat, aes(x = date, y = roll_sd_60)) +
  geom_line(linewidth = 0.35) +
  labs(title = "Rolling volatility (60d SD of returns)", x = NULL, y = "SD") +
  theme_minimal(base_size = 12)

p_dow = ggplot(feat |> dplyr::filter(!is.na(ret)), aes(x = dow, y = ret)) +
  geom_boxplot(outlier_alpha = 0.2) +
  labs(title = "Day-of-week effect (returns)", x = NULL, y = "Return") +
  theme_minimal(base_size = 12)

p_hist = ggplot(feat |> dplyr::filter(!is.na(ret)), aes(x = ret)) +
  geom_histogram(bins = 80) +
  labs(title = "Return distribution", x = "Return", y = "Count") +
  theme_minimal(base_size = 12)

p_qq = ggplot(feat |> dplyr::filter(!is.na(ret)), aes(sample = ret)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q plot (returns vs Normal)", x = "Theoretical", y = "Sample") +
  theme_minimal(base_size = 12)

ggsave(file.path(out_dir, "01_price.png"), p_price, width = 12, height = 4, dpi = 160)
ggsave(file.path(out_dir, "02_log_price.png"), p_log, width = 12, height = 4, dpi = 160)
ggsave(file.path(out_dir, "03_returns.png"), p_ret, width = 12, height = 4, dpi = 160)
ggsave(file.path(out_dir, "04_roll_vol_60.png"), p_vol, width = 12, height = 4, dpi = 160)
ggsave(file.path(out_dir, "05_dow_boxplot.png"), p_dow, width = 8, height = 4, dpi = 160)
ggsave(file.path(out_dir, "06_return_hist.png"), p_hist, width = 8, height = 4, dpi = 160)
ggsave(file.path(out_dir, "07_return_qq.png"), p_qq, width = 8, height = 4, dpi = 160)

# ---------------------------
# 8) Autocorrelation diagnostics
# ---------------------------
ret_vec = feat$ret
ret_vec = ret_vec[!is.na(ret_vec)]

png(file.path(out_dir, "08_acf_returns.png"), width = 1200, height = 650, res = 160)
acf(ret_vec, lag.max = 120, main = "ACF: returns")
dev.off()

png(file.path(out_dir, "09_pacf_returns.png"), width = 1200, height = 650, res = 160)
pacf(ret_vec, lag.max = 120, main = "PACF: returns")
dev.off()

png(file.path(out_dir, "10_acf_sq_returns.png"), width = 1200, height = 650, res = 160)
acf(ret_vec^2, lag.max = 120, main = "ACF: squared returns")
dev.off()

# Ljung-Box (serial correlation) on returns and squared returns
lb_ret = Box.test(ret_vec, lag = 20, type = "Ljung-Box")
lb_sq  = Box.test(ret_vec^2, lag = 20, type = "Ljung-Box")

# ARCH LM test for conditional heteroskedasticity
arch = FinTS::ArchTest(ret_vec, lags = 12)

diag_tbl = dplyr::tibble(
  test = c("Ljung-Box (ret, lag=20)", "Ljung-Box (ret^2, lag=20)", "ARCH LM (lags=12)"),
  statistic = c(unname(lb_ret$statistic), unname(lb_sq$statistic), unname(arch$statistic)),
  p_value = c(lb_ret$p.value, lb_sq$p.value, arch$p.value)
)
data.table::fwrite(diag_tbl, file.path(out_dir, "diagnostics_autocorr_arch.csv"))

# ---------------------------
# 9) Stationarity tests (levels vs returns)
# ---------------------------
# Price level is typically non-stationary; returns are often closer to stationary.
price_vec = feat$log_price
price_vec = price_vec[!is.na(price_vec)]

adf_price = tseries::adf.test(price_vec, alternative = "stationary", k = 12)
kpss_price = tseries::kpss.test(price_vec, null = "Level")

adf_ret = tseries::adf.test(ret_vec, alternative = "stationary", k = 12)
kpss_ret = tseries::kpss.test(ret_vec, null = "Level")

stat_tbl = dplyr::tibble(
  series = c("log_price", "log_price", "returns", "returns"),
  test = c("ADF", "KPSS(Level)", "ADF", "KPSS(Level)"),
  statistic = c(unname(adf_price$statistic), unname(kpss_price$statistic),
                unname(adf_ret$statistic), unname(kpss_ret$statistic)),
  p_value = c(adf_price$p.value, kpss_price$p.value, adf_ret$p.value, kpss_ret$p.value)
)
data.table::fwrite(stat_tbl, file.path(out_dir, "stationarity_tests.csv"))

# ---------------------------
# 10) Outliers (time-series aware)
# ---------------------------
# Work on log price; requires regular time series input for some routines.
# Convert to 'ts' using an approximate trading-day frequency (252) for diagnostics only.
# (This is not a strict calendar TS; it’s for outlier detection convenience.)
logp_non_na = feat |>
  dplyr::filter(!is.na(log_price)) |>
  dplyr::pull(log_price)

ts_logp = ts(logp_non_na, frequency = 252)

out = forecast::tsoutliers(ts_logp)
out_tbl = dplyr::tibble(
  index = out$index,
  type = out$type,
  time = out$time,
  effect = out$effect
)
data.table::fwrite(out_tbl, file.path(out_dir, "tsoutliers_log_price.csv"))

# ---------------------------
# 11) Structural breaks (regime changes)
# ---------------------------
# Breakpoints on log price trend
bp_price = strucchange::breakpoints(log_price ~ date, data = feat |> dplyr::filter(!is.na(log_price)))
bp_sum = summary(bp_price)
bp_dates = as.Date(feat$date[bp_price$breakpoints])

bp_tbl = dplyr::tibble(
  break_index = bp_price$breakpoints,
  break_date = bp_dates
)
data.table::fwrite(bp_tbl, file.path(out_dir, "breakpoints_log_price.csv"))

# Changepoint detection on returns (mean/variance)
# PELT is efficient for long series.
cpt = changepoint::cpt.meanvar(ret_vec, method = "PELT", penalty = "MBIC")
cpt_idx = changepoint::cpts(cpt)

cpt_tbl = dplyr::tibble(
  cpt_index = cpt_idx,
  cpt_date = feat$date[which(!is.na(feat$ret))][cpt_idx]
)
data.table::fwrite(cpt_tbl, file.path(out_dir, "changepoints_returns_meanvar.csv"))

# ---------------------------
# 12) Seasonality probing (aggregate to weekly)
# ---------------------------
# Daily equities can show weekly effects; also makes STL meaningful.
wk = feat |>
  dplyr::mutate(week = lubridate::floor_date(date, unit = "week", week_start = 1)) |>
  dplyr::group_by(week) |>
  dplyr::summarise(
    price = dplyr::last(AMZN),
    log_price = dplyr::last(log_price),
    ret_w = dplyr::last(log_price) - dplyr::lag(dplyr::last(log_price)),
    .groups = "drop"
  ) |>
  dplyr::arrange(week)

wk_ts = ts(wk$log_price, frequency = 52)

stl_fit = stl(wk_ts, s.window = "periodic", robust = TRUE)

png(file.path(out_dir, "11_stl_weekly_log_price.png"), width = 1200, height = 800, res = 160)
plot(stl_fit, main = "STL decomposition (weekly log price)")
dev.off()

# ---------------------------
# 13) Simple calendar-effect regression (returns ~ DOW + Month)
# ---------------------------
cal_reg = feat |>
  dplyr::filter(!is.na(ret)) |>
  dplyr::mutate(
    dow = factor(as.character(dow),
                 levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),
    month = factor(as.character(month),
                   levels = c("Jan","Feb","Mar","Apr","May","Jun",
                              "Jul","Aug","Sep","Oct","Nov","Dec"))
  )

m = lm(ret ~ dow + month, data = cal_reg)
reg_tbl = broom::tidy(m)
data.table::fwrite(reg_tbl, file.path(out_dir, "calendar_effects_lm.csv"))

# ---------------------------
# 14) One-shot report objects (RDS)
# ---------------------------
saveRDS(
  list(
    dt = dt,
    dt_full = dt_full,
    feat = feat,
    weekly = wk,
    summaries = list(price = summ_price, returns = summ_ret),
    tests = list(autocorr_arch = diag_tbl, stationarity = stat_tbl),
    breakpoints = bp_tbl,
    changepoints = cpt_tbl,
    calendar_lm = reg_tbl
  ),
  file = file.path(out_dir, "eda_objects.rds")
)

message("Done. Outputs written to: ", normalizePath(out_dir))

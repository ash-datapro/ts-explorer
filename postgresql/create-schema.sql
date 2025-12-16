-- 1) user + db
CREATE ROLE amzn_user WITH LOGIN PASSWORD 'timeseries1';

CREATE DATABASE amzn_ts OWNER amzn_user;

-- 2) connect to the new DB
\c amzn_ts

-- 3) schema
CREATE SCHEMA IF NOT EXISTS finance AUTHORIZATION amzn_user;

-- 4) table (simple and time-series friendly)
CREATE TABLE IF NOT EXISTS finance.amzn_daily (
  date  DATE PRIMARY KEY,
  amzn  NUMERIC(18,6)
);


-- 5) helpful index (primary key already indexes date; this is optional)
-- CREATE INDEX IF NOT EXISTS idx_amzn_daily_date ON finance.amzn_daily(date);

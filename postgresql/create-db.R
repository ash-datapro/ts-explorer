library(DBI)
library(RPostgres)
library(data.table)

csv_path = "~/Desktop/Project/ts-ex/data/input-dataset/amazon_stock.csv"  # adjust

x = fread(csv_path)
stopifnot(all(c("date", "AMZN") %in% names(x)))

x[, date := as.IDate(date)]
x[, AMZN := as.numeric(AMZN)]
setnames(x, "AMZN", "amzn")

con = dbConnect(
  RPostgres::Postgres(),
  dbname = "amzn_ts",
  host = "localhost",
  port = 5432,
  user = "amzn_user",
  password = "ash"
)

# Create schema/table (must be separate statements)
dbExecute(con, "CREATE SCHEMA IF NOT EXISTS finance;")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS finance.amzn_daily (
    date DATE PRIMARY KEY,
    amzn NUMERIC(18,6)
  );
")

# Stage -> upsert
dbWriteTable(
  con,
  Id(schema = "finance", table = "amzn_stage"),
  x,
  overwrite = TRUE
)

dbExecute(con, "
  INSERT INTO finance.amzn_daily(date, amzn)
  SELECT date, amzn
  FROM finance.amzn_stage
  ON CONFLICT (date) DO UPDATE
  SET amzn = EXCLUDED.amzn;
")

dbExecute(con, "DROP TABLE IF EXISTS finance.amzn_stage;")

dbDisconnect(con)

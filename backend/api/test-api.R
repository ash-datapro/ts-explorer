library(httr)
library(jsonlite)

base_url = "http://127.0.0.1:8000"

# health
r1 = GET(paste0(base_url, "/health"))
cat(content(r1, "text", encoding = "UTF-8"), "\n")

# meta
r2 = GET(paste0(base_url, "/meta"))
cat(content(r2, "text", encoding = "UTF-8"), "\n")

# predict latest date, all horizons
r3 = POST(
  paste0(base_url, "/predict"),
  body = list(),
  encode = "json"
)
cat(content(r3, "text", encoding = "UTF-8"), "\n")

# predict one horizon on a specific date
r4 = POST(
  paste0(base_url, "/predict"),
  body = list(horizon = 5, as_of = "2025-12-12"),
  encode = "json"
)
cat(content(r4, "text", encoding = "UTF-8"), "\n")

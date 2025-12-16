setwd("~/Desktop/Project/ts-explorer/backend/api/")

pr=plumber::pr('main.R')
pr$run(host = "0.0.0.0", port = 8000)

library(shiny)

setwd("~/Desktop/Project/ts-explorer/frontend")      # adjust path
Sys.setenv(PREDICT_API_URL = "http://127.0.0.1:8000/predict")
#source("utils.R")      # if utils.R is used directly by app.R; otherwise app.R can source it

shiny::runApp(".", host = "0.0.0.0", port = 8501)

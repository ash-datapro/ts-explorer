# Time-Series Explorer & Forecast Monitor

A production-style, end-to-end time-series ML system: **Postgres → leakage-safe feature engineering → rolling-origin backtests → deployable inference API (Plumber) → Shiny monitoring UI**.

Built to look and behave like a real ML service: reproducible runs, honest evaluation, auditable artifacts, and clear separation of **data / training / serving / monitoring**.

<!-- Badges-->

![R](https://img.shields.io/badge/R-%E2%89%A54.2-276DC3?logo=r&logoColor=white)
![tidymodels](https://img.shields.io/badge/tidymodels-ML%20workflow-1f77b4)
![xgboost](https://img.shields.io/badge/xgboost-forecasting-orange)
![Plumber](https://img.shields.io/badge/Plumber-API%20serving-0f766e)
![Shiny](https://img.shields.io/badge/Shiny-dashboard-0b7285)
![bslib](https://img.shields.io/badge/bslib-UI-334155)
![Postgres](https://img.shields.io/badge/PostgreSQL-data%20store-336791?logo=postgresql&logoColor=white)
![Docker](https://img.shields.io/badge/Docker-containerized-2496ED?logo=docker&logoColor=white)
![Compose](https://img.shields.io/badge/Docker%20Compose-multi--service-2496ED?logo=docker&logoColor=white)

<!-- ========================= -->
<!-- Badges: Engineering       -->
<!-- ========================= -->

![Leakage Safe](https://img.shields.io/badge/leakage-safe-brightgreen)
![Backtesting](https://img.shields.io/badge/eval-rolling--origin-blue)
![Baseline](https://img.shields.io/badge/baseline-zero--return-lightgrey)
![Artifacts](https://img.shields.io/badge/artifacts-auditable-blueviolet)
![API](https://img.shields.io/badge/interface-HTTP%20JSON-black)
![Observability](https://img.shields.io/badge/monitoring-Shiny%20UI-64748b)

**Quick links:** Demo · Architecture · Quickstart · API · ML pipeline · Dashboard · Configuration · Testing · Production notes

---

## Demo

Shiny dashboard for EDA + forecast calls + diagnostics, with a modular UI layout.

![](media/demo.gif)

---

## Why this exists

Time-series ML systems fail in predictable ways: leakage, unrealistic evaluation, unclear ownership of artifacts, and “demo-only” UIs.

This is a reference implementation of **correct time-series discipline** packaged as a **deployable service**:

* **Leakage-safe features** (only information available at time *t*)
* **Rolling-origin evaluation** (simulates live usage)
* **Hard baseline** (“do nothing / zero return”) so models must earn their keep
* **Auditable artifacts** (metrics + plots + model bundle)
* **Separation of concerns** (training ≠ serving ≠ monitoring)

---

## What’s included

### Core components

* **Postgres** — canonical source of truth for the time series
* **Training (R / tidymodels + xgboost)**
  Leak-free features, rolling-origin CV, tuning per horizon, artifact generation
* **Inference API (R / Plumber)**
  Loads a serialized model bundle (`backend/model.rds`) and serves predictions via HTTP (`/predict`)
* **Monitoring UI (R / Shiny + bslib)**
  Overview metrics, forecasting interface, and training diagnostics rendered from artifacts

### High-level architecture

```text
Postgres (source of truth)
        |
        v
Training (tidymodels + xgboost)
  - leakage-safe features
  - rolling-origin backtests
  - tuning per horizon
  - writes artifacts + model bundle
        |
        v
backend/model.rds + backend/reports/*
        |
        +--> Plumber API (/predict)
        |
        +--> Shiny UI (Overview / Forecast / Diagnostics / Explore / About)
```

---

## Quickstart

### Prerequisites

* Docker + Docker Compose

### 1) Configure environment

Create a `.env` in the repo root:

```bash
POSTGRES_USER=ts_user
POSTGRES_PASSWORD=ts_password
POSTGRES_DB=ts_db
```

### 2) Start the stack

```bash
docker compose up --build
```

Services:

* Postgres: `localhost:5432`
* Backend API: `http://localhost:8000`
* Shiny UI: `http://localhost:8501`

---

## Repository layout

Key locations you’ll interact with:

* `backend/` — Plumber API + model bundle + training artifacts
* `backend/model.rds` — serialized deployable model bundle
* `backend/reports/` — metrics/plots used by the Diagnostics tab
* `frontend/` — Shiny application

---

## API

### `POST /predict`

Forecasts **log return** at horizon `h` from an `as_of` date.

**Request**

```json
{
  "as_of": "2019-05-14",
  "horizon": 5
}
```

**Response (example)**

```json
{
  "as_of": "2019-05-14",
  "current_price": 1840.12,
  "prediction": {
    "h5": -0.0022
  }
}
```

**Interpretation**

* Target is **log return** for horizon `h`
* UI converts to implied percent move via `exp(r) - 1`

**Smoke test**

```bash
curl -X POST http://localhost:8000/predict \
  -H "Content-Type: application/json" \
  -d '{"as_of":"2019-05-14","horizon":5}'
```

---

## Local development (no Docker)

### Postgres

Run Postgres however you prefer. Ensure env vars are set:

```bash
export DB_USER=ts_user
export DB_PASSWORD=ts_password
export DB_HOST=127.0.0.1
export DB_PORT=5432
export DB_NAME=ts_db
```

### Backend API (Plumber)

From `backend/`:

```r
plumber::pr_run(plumber::plumb("api/main.R"), host = "0.0.0.0", port = 8000)
```

### Frontend (Shiny)

From `frontend/`:

```r
shiny::runApp(".", host = "0.0.0.0", port = 8501)
```

The UI calls the backend via:

* `PREDICT_API_URL` (default: `http://127.0.0.1:8000/predict`)
* In Docker Compose this is typically: `http://backend:8000/predict`

---

## ML pipeline

This project demonstrates “time-series ML done right”:

1. **Reproducible run** — fixed seed, explicit horizons, rolling-backtest settings
2. **Leakage-safe features** — lagged returns, rolling stats, momentum-style sums, simple calendar signals
3. **Direct multi-horizon forecasting** — one supervised problem per horizon (e.g., 1/5/20 days)
4. **Rolling-origin CV** — repeatedly train on past, test on next window, moving forward through time
5. **Hard baseline** — predicts zero return and reports RMSE/MAE so the model must beat “do nothing”
6. **Production-style preprocessing** — imputation/encoding, drop constant cols, numeric scaling
7. **Tuning inside evaluation** — fixed-budget random search; select by RMSE and report MAE
8. **Final training + artifacts** — final per-horizon training, then save model bundle + reports
9. **Sanity diagnostics** — backtest RMSE by horizon + actual-vs-pred plots

**Artifacts**

* `backend/reports/`
* `backend/model.rds`

---

## Dashboard guide

### Overview

* KPI tiles: price / daily return / 20d annualized vol / max drawdown
* Core plots: price, returns, vol, drawdown
* Recent observations table

### Forecast

* Sends request to `/predict`
* Shows predicted return and implied move context

### Diagnostics

Renders training outputs from `backend/reports/`:

* `backtest_rmse_by_horizon.png`
* `actual_vs_pred.png`
* `backtest_metrics.csv`

> In Docker Compose, this works because the frontend container mounts `./backend/reports` read-only.

### Explore (bring your own CSV)

Upload a time-series CSV for:

* date parsing + missingness + duplicates
* coverage + frequency hints
* returns/vol regimes, ACF, basic diagnostics
* “safe defaults” that are useful without overclaiming

---

## Configuration

### Environment variables

Backend:

* `DB_USER`, `DB_PASSWORD`, `DB_HOST`, `DB_PORT`, `DB_NAME`

Frontend:

* `PREDICT_API_URL` (default: `http://127.0.0.1:8000/predict`)
* `DB_*` (only if the Shiny app loads directly from Postgres)

---

## Testing

If present, run from each folder:

* `frontend/test-frontend.R`
* `backend/test-api.R`

You can always smoke-test the API with the curl example in the API section.

---

## Production notes

If you were taking this beyond localhost:

* Add structured logging (request IDs, latency, model version, feature coverage)
* Add model registry semantics (metadata + schema + training hash)
* Add monitoring: prediction distribution drift, null rates, feature ranges, endpoint SLOs
* Add CI: lint + unit tests + minimal integration test (compose spin-up + API call)
* Add auth + rate limiting for public exposure
* Consider offline/online parity checks if you scale beyond a single series

---

## Disclaimer

Forecasting returns is inherently noisy. This project is for engineering/learning purposes and is not financial advice.

---

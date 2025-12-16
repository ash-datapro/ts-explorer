# backend/Dockerfile
# R + Plumber API for AMZN time-series service

FROM rocker/r-ver:4.3.2

# ---- system deps (RPostgres, xgboost compilation, common R packages) ----
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    build-essential \
    cmake \
    git \
    && rm -rf /var/lib/apt/lists/*

# ---- copy requirements first for better docker layer caching ----
WORKDIR /app
COPY api/requirements /app/api/requirements

# Install R packages listed in api/requirements
RUN R -e "pkgs = readLines('/app/api/requirements'); pkgs = pkgs[nzchar(pkgs)]; install.packages(pkgs, repos='https://cloud.r-project.org');"

# ---- copy the rest of the backend ----
COPY . /app

# Run the API from /app/api so main.R can read ./model.rds
WORKDIR /app/api

ENV PORT=8000
EXPOSE 8000

CMD ["R", "-e", "pr = plumber::pr('main.R'); pr$run(host='0.0.0.0', port=as.integer(Sys.getenv('PORT','8000')))"]

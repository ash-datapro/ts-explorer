FROM rocker/r-ver:4.3.2

# System dependencies commonly needed by tidyverse/httr/jsonlite/plotly/DT
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy only requirements first (better layer caching)
WORKDIR /app
COPY requirements.txt /app/requirements.txt

# Install R packages listed in requirements.txt
RUN R -e "options(repos = c(CRAN='https://cloud.r-project.org')); \
          pkgs = readLines('requirements.txt'); \
          pkgs = pkgs[nchar(pkgs) > 0 & !grepl('^#', pkgs)]; \
          install.packages(pkgs);"

# Copy the rest of the frontend app (app.R + R/ modules)
COPY . /app

EXPOSE 8501

CMD ["R", "-e", "options(shiny.port=8501, shiny.host='0.0.0.0'); shiny::runApp('/app', launch.browser=FALSE)"]

# syntax=docker/dockerfile:1.6
# =============================================================================
#  ICES VMS & Logbook Explorer
#
#  Stage 1 (python)  simulate an ICES-wide Table 1 / Table 2 Parquet lake
#  Stage 2 (R)       build the DuckDB cube warehouse, install the Shiny app
#
#  Build:  docker build -t ices-vms-explorer .
#          docker build --build-arg ROWS_PER_YEAR=2000000 -t ices-vms-explorer:xl .
#  Run:    docker run --rm -p 3838:3838 ices-vms-explorer   ->  http://localhost:3838
# =============================================================================

# ---------------------------------------------------------------- stage 1: data
FROM python:3.12-slim AS data
RUN pip install --no-cache-dir "numpy>=2.0" "duckdb==1.3.2" "pyarrow>=16" "scipy>=1.12" "global-land-mask==1.0.0"
WORKDIR /sim
COPY sim/simulate.py .
# ~ rows of Table 1 per year across all countries (600k ~= 8M rows over 13 years)
ARG ROWS_PER_YEAR=600000
ARG YEARS=2013-2025
ARG SEED=20261006
ENV SIM_DUCKDB_MEM=1GB
RUN python simulate.py --out /data --years ${YEARS} --rows-per-year ${ROWS_PER_YEAR} --seed ${SEED}

# ---------------------------------------------------------------- stage 2: app
FROM rocker/r-ver:4.5.1 AS app
ENV DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8 LC_ALL=C.UTF-8 \
    DATA_DIR=/data \
    DUCKDB_MEMORY=2GB \
    CACHE_MB=512 \
    SQL_CONSOLE=true

# Posit Package Manager serves pre-built Linux binaries. Installs run in
# separate cached layers with a long timeout + retries (big binaries such as
# duckdb can exceed R's default 60 s download timeout).
COPY docker/install_packages.R /opt/build/install_packages.R
RUN Rscript /opt/build/install_packages.R DBI duckdb
RUN Rscript /opt/build/install_packages.R jsonlite rlang cachem base64enc htmltools
RUN Rscript /opt/build/install_packages.R shiny bslib bsicons
RUN Rscript /opt/build/install_packages.R leaflet leaflet.providers DT
RUN Rscript /opt/build/install_packages.R ggplot2 plotly

# Parquet lake from stage 1, then the hot tier (cubes) built with the same
# DuckDB version the app uses -> no storage-format mismatch.
COPY --from=data /data/lake /data/lake
COPY sim/build_warehouse.R /opt/build/build_warehouse.R
RUN Rscript /opt/build/build_warehouse.R /data

COPY app /app
RUN useradd --create-home --uid 1001 shiny && chown -R shiny:shiny /app && chmod -R a+rX /data
USER shiny
WORKDIR /app
EXPOSE 3838
CMD ["Rscript", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838, launch.browser = FALSE)"]

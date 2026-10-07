#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# Build the "hot" tier: a DuckDB warehouse of pre-aggregated cubes on top of the
# Parquet lake written by simulate.py.
#
#   lake/vms/Year=*/CountryCode=*/part-0.parquet      Table 1 (c-square)   cold
#   lake/logbook/Year=*/CountryCode=*/part-0.parquet  Table 2 (rectangle)  cold
#   warehouse.duckdb                     cubes + dimension tables             hot
#
# Cube pyramid (VMS): the 0.05 deg c-square grid is rolled up to 0.25, 0.5 and
# 1 degree (0.1 is derived from the 0.05 cube for the visible bbox only). The map always asks the level whose cells are a few pixels wide
# at the current zoom, so a whole-ICES view scans ~100k rows, not millions.
# Dimensions in the cubes are DuckDB ENUMs (1-byte dictionary codes) and the
# high-cardinality, map-irrelevant dims (habitat, depth, metier L6) are dropped.
# -----------------------------------------------------------------------------
suppressPackageStartupMessages({ library(DBI); library(duckdb) })

args     <- commandArgs(trailingOnly = TRUE)
data_dir <- normalizePath(if (length(args)) args[1] else "data", mustWork = TRUE)
lake     <- file.path(data_dir, "lake")
db_path  <- file.path(data_dir, "warehouse.duckdb")
if (file.exists(db_path)) file.remove(db_path)

con <- dbConnect(duckdb(), db_path)
on.exit(dbDisconnect(con, shutdown = TRUE))
x <- function(sql) invisible(dbExecute(con, sql))
q <- function(sql) dbGetQuery(con, sql)
t0 <- Sys.time()
tick <- function(msg) cat(sprintf("[%6.1fs] %s\n", as.numeric(difftime(Sys.time(), t0, units = "secs")), msg))

vms_glob <- file.path(lake, "vms", "*", "*", "*.parquet")
le_glob  <- file.path(lake, "logbook", "*", "*", "*.parquet")
x(sprintf("CREATE TEMP VIEW vms AS SELECT * FROM read_parquet('%s', hive_partitioning = true)", vms_glob))
x(sprintf("CREATE TEMP VIEW le  AS SELECT * FROM read_parquet('%s', hive_partitioning = true)", le_glob))

# ---- ENUM dictionaries ------------------------------------------------------
enum_vals <- function(col) {
  v <- q(sprintf("SELECT DISTINCT %s AS v FROM (SELECT %s FROM vms UNION ALL SELECT %s FROM le) WHERE %s IS NOT NULL ORDER BY 1",
                 col, col, col, col))$v
  paste0("'", gsub("'", "''", v), "'", collapse = ", ")
}
x(sprintf("CREATE TYPE country_t AS ENUM (%s)", enum_vals("CountryCode")))
x(sprintf("CREATE TYPE gear_t    AS ENUM (%s)", enum_vals("MetierL4")))
x(sprintf("CREATE TYPE target_t  AS ENUM (%s)", enum_vals("MetierL5")))
x(sprintf("CREATE TYPE vlen_t    AS ENUM (%s)", enum_vals("VesselLengthRange")))
tick("enum types")

# ---- VMS cube pyramid --------------------------------------------------------
# (0.1 deg is served from cube_1 on the fly: the map only asks it for a bbox)
levels <- c(`1` = 0.05, `5` = 0.25, `10` = 0.5, `20` = 1.0)
for (k in names(levels)) {
  kk <- as.integer(k)
  src <- if (kk == 1) "vms" else "cube_1"
  sel <- if (kk == 1) {
    "Year::SMALLINT AS yr, Month::TINYINT AS mo, CountryCode::country_t AS c, MetierL4::gear_t AS g,
     MetierL5::target_t AS t, VesselLengthRange::vlen_t AS v, ix::SMALLINT AS x, iy::SMALLINT AS y,
     sum(FishingHour)::REAL AS h, sum(kWFishingHour)::REAL AS kwh, sum(coalesce(SweptArea,0))::REAL AS sa,
     sum(TotWeight)::REAL AS kg, sum(TotValue)::REAL AS eur, sum(NumberOfRecords)::INTEGER AS n,
     sum(NoDistinctVessels)::INTEGER AS nv"
  } else {
    sprintf("yr, mo, c, g, t, v, (x // %d)::SMALLINT AS x, (y // %d)::SMALLINT AS y,
     sum(h)::REAL AS h, sum(kwh)::REAL AS kwh, sum(sa)::REAL AS sa, sum(kg)::REAL AS kg,
     sum(eur)::REAL AS eur, sum(n)::INTEGER AS n, sum(nv)::INTEGER AS nv", kk, kk)
  }
  x(sprintf("CREATE TABLE cube_%s AS SELECT %s FROM %s GROUP BY ALL ORDER BY yr, y, x", k, sel, src))
  n <- q(sprintf("SELECT count(*) n FROM cube_%s", k))$n
  tick(sprintf("cube_%-2s (%.2f deg): %s rows", k, levels[[k]], format(n, big.mark = ",")))
}

# non-spatial cube for time series / composition charts (tiny, scans in ~ms)
x("CREATE TABLE cube_ts AS SELECT yr, mo, c, g, t, v, sum(h)::REAL AS h, sum(kwh)::REAL AS kwh,
   sum(sa)::REAL AS sa, sum(kg)::REAL AS kg, sum(eur)::REAL AS eur, sum(n)::INTEGER AS n, sum(nv)::INTEGER AS nv,
   count(*)::INTEGER AS cells FROM cube_1 GROUP BY ALL ORDER BY yr, mo")
tick(sprintf("cube_ts: %s rows", format(q("SELECT count(*) n FROM cube_ts")$n, big.mark = ",")))

# ---- logbook cube (ICES rectangle grid: 1 x 0.5 deg, origin -50E / 36N) ----
rect_sql <- "
  (substr(ICESrectangle,1,2)::INT - 1) AS ry,
  CASE WHEN substr(ICESrectangle,3,1) = 'A' THEN substr(ICESrectangle,4,1)::INT + 6
       ELSE (strpos('ABCDEFGHJKLM', substr(ICESrectangle,3,1)) - 1) * 10 + substr(ICESrectangle,4,1)::INT END AS rx"
x(sprintf("
  CREATE TABLE le_cube AS
  SELECT Year::SMALLINT AS yr, Month::TINYINT AS mo, CountryCode::country_t AS c, MetierL4::gear_t AS g,
         MetierL5::target_t AS t, VesselLengthRange::vlen_t AS v, (VMSEnabled = 'Y') AS vms,
         ICESrectangle AS rect, %s,
         sum(FishingDays)::REAL AS days, sum(kWFishingDays)::REAL AS kwd, sum(TotWeight)::REAL AS kg,
         sum(TotValue)::REAL AS eur, sum(NoDistinctVessels)::INTEGER AS nv
  FROM le GROUP BY ALL ORDER BY yr, ry, rx", rect_sql))
tick(sprintf("le_cube: %s rows", format(q("SELECT count(*) n FROM le_cube")$n, big.mark = ",")))

x(sprintf("
  CREATE TABLE vms_rect AS
  SELECT Year::SMALLINT AS yr, Month::TINYINT AS mo, CountryCode::country_t AS c, MetierL4::gear_t AS g,
         MetierL5::target_t AS t, VesselLengthRange::vlen_t AS v, ICESrectangle AS rect, %s,
         sum(FishingHour)::REAL AS h, sum(TotWeight)::REAL AS kg, sum(TotValue)::REAL AS eur
  FROM vms GROUP BY ALL ORDER BY yr, ry, rx", rect_sql))
tick("vms_rect")

# ---- dimension / metadata tables ---------------------------------------------
x("CREATE TABLE dim_metier AS
   SELECT MetierL4, MetierL5, MetierL6, sum(FishingHour) AS h FROM vms GROUP BY ALL ORDER BY h DESC")
x("CREATE TABLE lake_stats AS
   SELECT 'vms' AS tbl, Year, count(*) AS rows FROM vms GROUP BY ALL
   UNION ALL SELECT 'logbook', Year, count(*) FROM le GROUP BY ALL ORDER BY 1, 2")
meta <- paste(readLines(file.path(lake, "_meta.json"), warn = FALSE), collapse = "\n")
x(sprintf("CREATE TABLE meta AS SELECT %s AS json", dbQuoteString(con, meta)))
x("CHECKPOINT")
tick(sprintf("warehouse written: %s (%.0f MB)", db_path, file.size(db_path) / 1e6))

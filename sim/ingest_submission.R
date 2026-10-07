#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# Load REAL ICES data call tables into the explorer's lake layout.
#
# Takes the files produced by the national workflow (3_data_submission.R):
#   table1Save.csv / .rds   (VMS, one row per C-square x month x metier x ...)
#   table2Save.csv / .rds   (logbook, one row per ICES rectangle x month x ...)
# and writes
#   <out>/lake/vms/Year=YYYY/CountryCode=XX/part-0.parquet
#   <out>/lake/logbook/Year=YYYY/CountryCode=XX/part-0.parquet
#   <out>/lake/_meta.json
# Then run:  Rscript build_warehouse.R <out>
#
# Usage:
#   Rscript ingest_submission.R --table1 table1Save.csv --table2 table2Save.csv --out data
#   (several files per flag may be comma-separated, e.g. one per country)
# -----------------------------------------------------------------------------
suppressPackageStartupMessages({ library(DBI); library(duckdb) })

arg <- function(flag, default = NULL) {
  a <- commandArgs(trailingOnly = TRUE); i <- match(flag, a)
  if (is.na(i)) default else a[i + 1]
}
t1_files <- strsplit(arg("--table1", ""), ",")[[1]]
t2_files <- strsplit(arg("--table2", ""), ",")[[1]]
out <- arg("--out", "data")
stopifnot(length(t1_files) > 0, length(t2_files) > 0)

con <- dbConnect(duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE))
x <- function(sql) invisible(dbExecute(con, sql))

load_tbl <- function(files, name) {
  parts <- lapply(seq_along(files), function(i) {
    f <- files[i]; nm <- sprintf("%s_%d", name, i)
    if (grepl("\\.rds$", f, ignore.case = TRUE)) {
      d <- as.data.frame(readRDS(f))
      names(d)[names(d) == "Csquare"] <- "C-square"
      names(d)[names(d) == "Habitat"] <- "HabitatType"
      names(d)[names(d) == "Depth"] <- "DepthRange"
      names(d)[names(d) == "No_Records"] <- "NumberOfRecords"
      duckdb::duckdb_register(con, nm, d)
    } else {
      # all_varchar: the sniffer would otherwise read country code "NO" (Norway!) as a boolean
      x(sprintf("CREATE TEMP VIEW %s AS SELECT * FROM read_csv('%s', header = true, all_varchar = true)", nm, f))
    }
    nm
  })
  x(sprintf("CREATE TEMP VIEW %s AS %s", name, paste(sprintf("SELECT * FROM %s", unlist(parts)), collapse = " UNION ALL BY NAME ")))
}
load_tbl(t1_files, "raw1")
load_tbl(t2_files, "raw2")

# C-square (0.05 deg) -> centre lon/lat, grid indices, ICES rectangle, Morton key
x("CREATE MACRO m8(v) AS ((v | (v << 8)) & 16711935)")
x("CREATE MACRO m4(v) AS ((v | (v << 4)) & 252645135)")
x("CREATE MACRO m2(v) AS ((v | (v << 2)) & 858993459)")
x("CREATE MACRO m1(v) AS ((v | (v << 1)) & 1431655765)")
x("CREATE MACRO morton(a, b) AS m1(m2(m4(m8(a & 65535)))) | (m1(m2(m4(m8(b & 65535)))) << 1)")
x("CREATE MACRO rect_of(lon, lat) AS
     lpad((floor((lat - 36) * 2)::INT + 1)::VARCHAR, 2, '0') ||
     CASE WHEN lon < -40 THEN 'A' ELSE substr('BCDEFGHJKLM', floor((lon + 40) / 10)::INT + 1, 1) END ||
     CASE WHEN lon < -40 THEN (floor(lon + 44)::INT % 10)::VARCHAR ELSE (((floor(lon)::INT % 10) + 10) % 10)::VARCHAR END")
x("
CREATE TEMP TABLE t1 AS
WITH s AS (
  SELECT *, string_split(\"C-square\", ':') AS p FROM raw1
), d AS (
  SELECT *,
    substr(p[1], 1, 1)::INT AS q, substr(p[1], 2, 1)::INT AS lat10, substr(p[1], 3, 2)::INT AS lon10,
    substr(p[2], 2, 1)::INT AS lat1, substr(p[2], 3, 1)::INT AS lon1,
    substr(p[3], 2, 1)::INT AS lat01, substr(p[3], 3, 1)::INT AS lon01, p[4]::INT AS q5
  FROM s
), c AS (
  SELECT *,
    (lat10 * 10 + lat1 + lat01 / 10 + CASE WHEN q5 IN (3, 4) THEN 0.05 ELSE 0 END + 0.025) * CASE WHEN q IN (3, 5) THEN -1 ELSE 1 END AS lat_,
    (lon10 * 10 + lon1 + lon01 / 10 + CASE WHEN q5 IN (2, 4) THEN 0.05 ELSE 0 END + 0.025) * CASE WHEN q IN (5, 7) THEN -1 ELSE 1 END AS lon_
  FROM d
)
SELECT 'VE' AS RecordType, CountryCode::VARCHAR AS CountryCode, Year::SMALLINT AS Year, Month::TINYINT AS Month,
       \"C-square\" AS Csquare, rect_of(lon_, lat_) AS ICESrectangle,
       floor((lon_ + 50) / 0.05)::SMALLINT AS ix, floor((lat_ - 34) / 0.05)::SMALLINT AS iy,
       lon_::REAL AS lon, lat_::REAL AS lat,
       MetierL4, MetierL5, MetierL6, VesselLengthRange, HabitatType::VARCHAR AS HabitatType, DepthRange::VARCHAR AS DepthRange,
       NumberOfRecords::INTEGER AS NumberOfRecords, AverageFishingSpeed::REAL AS AverageFishingSpeed,
       FishingHour::REAL AS FishingHour, AverageInterval::REAL AS AverageInterval, AverageVesselLength::REAL AS AverageVesselLength,
       AveragekW::REAL AS AveragekW, kWFishingHour::REAL AS kWFishingHour, SweptArea::REAL AS SweptArea,
       TotWeight::REAL AS TotWeight, TotValue::REAL AS TotValue, NoDistinctVessels::SMALLINT AS NoDistinctVessels,
       AnonymizedVesselID::VARCHAR AS AnonymizedVesselID, AverageGearWidth::REAL AS AverageGearWidth,
       morton(floor((lon_ + 50) / 0.05)::BIGINT, floor((lat_ - 34) / 0.05)::BIGINT) AS _z
FROM c")

x("CREATE TEMP TABLE t2 AS SELECT 'LE' AS RecordType, CountryCode::VARCHAR AS CountryCode, Year::SMALLINT AS Year,
     Month::TINYINT AS Month, ICESrectangle, MetierL4, MetierL5, MetierL6, VesselLengthRange, VMSEnabled,
     FishingDays::REAL AS FishingDays, kWFishingDays::REAL AS kWFishingDays, TotWeight::REAL AS TotWeight,
     TotValue::REAL AS TotValue, NoDistinctVessels::SMALLINT AS NoDistinctVessels, AnonymizedVesselID::VARCHAR AS AnonymizedVesselID
   FROM raw2")

write_parts <- function(tbl, dir, order, rg) {
  keys <- dbGetQuery(con, sprintf("SELECT DISTINCT Year, CountryCode FROM %s ORDER BY 1, 2", tbl))
  for (i in seq_len(nrow(keys))) {
    d <- file.path(out, "lake", dir, sprintf("Year=%d", keys$Year[i]), sprintf("CountryCode=%s", keys$CountryCode[i]))
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    x(sprintf("COPY (SELECT * EXCLUDE (Year, CountryCode%s) FROM %s WHERE Year = %d AND CountryCode = '%s' ORDER BY %s)
               TO '%s/part-0.parquet' (FORMAT parquet, COMPRESSION zstd, ROW_GROUP_SIZE %d)",
              if (tbl == "t1") ", _z" else "", tbl, keys$Year[i], keys$CountryCode[i], order, d, rg))
  }
  keys
}
k1 <- write_parts("t1", "vms", "_z", 8192)
k2 <- write_parts("t2", "logbook", "ICESrectangle, Month", 16384)

stats <- dbGetQuery(con, "SELECT Year AS year, (SELECT count(*) FROM t1 a WHERE a.Year = y.Year) AS vms_rows,
                           (SELECT count(*) FROM t2 b WHERE b.Year = y.Year) AS logbook_rows
                          FROM (SELECT DISTINCT Year FROM t1) y ORDER BY 1")
meta <- list(generated = format(Sys.time()), source = "real submission", grid = list(res = 0.05, lon0 = -50, lat0 = 34),
             years = stats$year, stats = stats, injected_issues = list(), wind_farms = list(), bottom_closures = list())
writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE), file.path(out, "lake", "_meta.json"))
cat(sprintf("wrote %d VMS and %d logbook partitions to %s/lake\n", nrow(k1), nrow(k2), out))

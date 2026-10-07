# -----------------------------------------------------------------------------
# Data access layer
#
#   * One in-memory DuckDB process per R process. The warehouse (hot tier,
#     pre-aggregated cubes) is ATTACHed read-only; the Parquet lake (cold tier,
#     full ICES table 1 / table 2) is exposed through views.
#   * Every query goes through q(): results are memoised in an LRU memory cache
#     keyed on the SQL text, and each call is timed into a ring buffer that
#     drives the performance HUD and the query log on the "Under the hood" tab.
# -----------------------------------------------------------------------------

DATA_DIR <- normalizePath(Sys.getenv("DATA_DIR", "../data"), mustWork = FALSE)
if (!dir.exists(DATA_DIR)) DATA_DIR <- normalizePath("data", mustWork = FALSE)

GRID <- list(res = 0.05, lon0 = -50, lat0 = 34)
RECT <- list(lon0 = -50, lat0 = 36, dx = 1, dy = 0.5)
LEVELS <- c(1L, 2L, 5L, 10L, 20L) # multiples of 0.05 deg in the cube pyramid

db_connect <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  threads <- Sys.getenv("DUCKDB_THREADS", "")
  if (nzchar(threads)) DBI::dbExecute(con, sprintf("SET threads = %s", threads))
  DBI::dbExecute(con, sprintf("SET memory_limit = '%s'", Sys.getenv("DUCKDB_MEMORY", "2GB")))
  DBI::dbExecute(con, sprintf("ATTACH '%s' AS wh (READ_ONLY)", file.path(DATA_DIR, "warehouse.duckdb")))
  DBI::dbExecute(con, "USE wh")
  lake <- file.path(DATA_DIR, "lake")
  DBI::dbExecute(con, sprintf(
    "CREATE OR REPLACE TEMP VIEW vms AS SELECT * FROM read_parquet('%s/vms/*/*/*.parquet', hive_partitioning = true)", lake))
  DBI::dbExecute(con, sprintf(
    "CREATE OR REPLACE TEMP VIEW logbook AS SELECT * FROM read_parquet('%s/logbook/*/*/*.parquet', hive_partitioning = true)", lake))
  # Harden for the SQL console: only the data directory is readable, no writes
  # elsewhere, and the configuration is locked so a query cannot undo this.
  ok <- tryCatch({
    DBI::dbExecute(con, sprintf("SET allowed_directories = ['%s/']", DATA_DIR))
    DBI::dbExecute(con, "SET enable_external_access = false")
    DBI::dbExecute(con, "SET lock_configuration = true")
    TRUE
  }, error = function(e) { message("DuckDB hardening unavailable: ", conditionMessage(e)); FALSE })
  attr(con, "hardened") <- ok
  con
}

CON <- db_connect()
SQL_CONSOLE <- isTRUE(attr(CON, "hardened")) && Sys.getenv("SQL_CONSOLE", "true") != "false"

# ---- cache + query log --------------------------------------------------------
QCACHE <- cachem::cache_mem(max_size = as.numeric(Sys.getenv("CACHE_MB", "512")) * 1024^2, evict = "lru")
QLOG <- new.env()
QLOG$rows <- vector("list", 0)
QLOG$hits <- 0L
QLOG$miss <- 0L

log_query <- function(label, ms, n, cached, sql) {
  QLOG$rows <- c(list(list(time = format(Sys.time(), "%H:%M:%OS2"), label = label, ms = round(ms, 1),
                           rows = n, cached = cached, sql = sql)), utils::head(QLOG$rows, 199))
  if (cached) QLOG$hits <- QLOG$hits + 1L else QLOG$miss <- QLOG$miss + 1L
}

#' Run a query (memoised). Returns a data.frame with attributes ms & cached.
q <- function(sql, label = "query", cache = TRUE) {
  key <- rlang::hash(sql)
  t0 <- proc.time()[[3]]
  if (cache) {
    hit <- QCACHE$get(key)
    if (!cachem::is.key_missing(hit)) {
      ms <- (proc.time()[[3]] - t0) * 1000
      log_query(label, ms, nrow(hit), TRUE, sql)
      attr(hit, "ms") <- ms
      attr(hit, "cached") <- TRUE
      return(hit)
    }
  }
  res <- DBI::dbGetQuery(CON, sql)
  ms <- (proc.time()[[3]] - t0) * 1000
  if (cache) QCACHE$set(key, res)
  log_query(label, ms, nrow(res), FALSE, sql)
  attr(res, "ms") <- ms
  attr(res, "cached") <- FALSE
  res
}

# ---- SQL building helpers -----------------------------------------------------
sq <- function(x) paste0("'", gsub("'", "''", as.character(x), fixed = TRUE), "'")
in_list <- function(col, vals) if (length(vals)) sprintf("%s IN (%s)", col, paste(sq(vals), collapse = ", ")) else NULL

# Column names differ between cubes (short) and the lake (ICES field names)
COLS <- list(
  cube = list(yr = "yr", mo = "mo", c = "c", g = "g", t = "t", v = "v"),
  lake = list(yr = "Year", mo = "Month", c = "CountryCode", g = "MetierL4", t = "MetierL5", v = "VesselLengthRange")
)

#' WHERE clause for the global filter set.
where_sql <- function(f, src = "cube", years = TRUE, extra = NULL) {
  cl <- COLS[[src]]
  parts <- c(
    if (years && length(f$years) == 2) sprintf("%s BETWEEN %d AND %d", cl$yr, f$years[1], f$years[2]),
    if (length(f$months)) sprintf("%s IN (%s)", cl$mo, paste(as.integer(f$months), collapse = ",")),
    in_list(cl$c, f$c), in_list(cl$g, f$g), in_list(cl$t, f$t), in_list(cl$v, f$v),
    extra
  )
  if (length(parts)) paste("WHERE", paste(parts, collapse = " AND ")) else ""
}

METRICS <- list(
  h    = list(label = "Fishing hours",        unit = "h",        cube = "sum(h)",            lake = "sum(FishingHour)",   d = 0),
  kwh  = list(label = "kW fishing hours",     unit = "kWh",      cube = "sum(kwh)",          lake = "sum(kWFishingHour)", d = 0),
  sa   = list(label = "Swept area",           unit = "km²", cube = "sum(sa)",           lake = "sum(SweptArea)",     d = 1),
  kg   = list(label = "Landings",             unit = "t",        cube = "sum(kg)/1000",      lake = "sum(TotWeight)/1000", d = 1),
  eur  = list(label = "Landed value",         unit = "k€",  cube = "sum(eur)/1000",     lake = "sum(TotValue)/1000",  d = 1),
  lpue = list(label = "LPUE",                 unit = "kg/h",     cube = "sum(kg)/nullif(sum(h),0)",  lake = "sum(TotWeight)/nullif(sum(FishingHour),0)", d = 1, ratio = TRUE),
  vpue = list(label = "VPUE",                 unit = "€/h", cube = "sum(eur)/nullif(sum(h),0)", lake = "sum(TotValue)/nullif(sum(FishingHour),0)",  d = 1, ratio = TRUE),
  n    = list(label = "VMS pings",            unit = "pings",    cube = "sum(n)",            lake = "sum(NumberOfRecords)", d = 0)
)
metric_choices <- setNames(names(METRICS), vapply(METRICS, `[[`, "", "label"))

#' metric expression, optionally restricted with an aggregate FILTER clause
mexpr <- function(m, src = "cube", filter = NULL) {
  e <- METRICS[[m]][[src]]
  if (is.null(filter)) return(e)
  gsub("sum\\(([^()]*)\\)", sprintf("sum(\\1) FILTER (WHERE %s)", filter), e)
}

level_deg <- function(k) k * GRID$res

#' Physical source for a pyramid level. 0.1 deg (k = 2) has no table of its own:
#' it is rolled up from cube_1 on the fly, which is cheap because the map only
#' asks for it within the (padded) viewport.
cube_ref <- function(k) {
  if (k == 2L) list(tbl = "cube_1", x = "(x // 2)", y = "(y // 2)", f = 2L)
  else list(tbl = sprintf("cube_%d", k), x = "x", y = "y", f = 1L)
}
#' bbox predicate on the physical table for cells (x0..x1, y0..y1) at level k
bbox_sql <- function(bb, k) {
  r <- cube_ref(k)
  sprintf("x BETWEEN %d AND %d AND y BETWEEN %d AND %d", bb$x0 * r$f, bb$x1 * r$f + r$f - 1L, bb$y0 * r$f, bb$y1 * r$f + r$f - 1L)
}

#' Pick the cube level for a Leaflet zoom so that cells are >= ~2.5 px wide.
level_for_zoom <- function(z) {
  if (is.null(z)) return(10L)
  px_per_deg <- 256 * 2^z / 360
  for (k in LEVELS) if (level_deg(k) * px_per_deg >= 2.5) return(k)
  20L
}

#' Bounding box (lon/lat) -> integer cube cell range at level k, padded.
bbox_cells <- function(b, k, pad = 0.5) {
  w <- b$east - b$west; h <- b$north - b$south
  west <- b$west - w * pad; east <- b$east + w * pad
  south <- b$south - h * pad; north <- b$north + h * pad
  d <- level_deg(k)
  list(x0 = floor((west - GRID$lon0) / d), x1 = floor((east - GRID$lon0) / d),
       y0 = floor((south - GRID$lat0) / d), y1 = floor((north - GRID$lat0) / d),
       west = west, east = east, south = south, north = north)
}

#' Encode numeric vectors as base64 little-endian typed arrays for the browser.
b64_i16 <- function(x) base64enc::base64encode(writeBin(as.integer(x), raw(), size = 2, endian = "little"))
b64_f32 <- function(x) base64enc::base64encode(writeBin(as.numeric(x), raw(), size = 4, endian = "little"))

# ---- dimension values for the UI ---------------------------------------------
DIMS <- local({
  yrs <- q("SELECT min(yr) a, max(yr) b FROM cube_ts", "init")
  list(
    years = c(yrs$a, yrs$b),
    c = q("SELECT c::VARCHAR AS v, sum(h) h FROM cube_ts GROUP BY 1 ORDER BY h DESC", "init")$v,
    g = q("SELECT g::VARCHAR AS v, sum(h) h FROM cube_ts GROUP BY 1 ORDER BY h DESC", "init")$v,
    t = q("SELECT t::VARCHAR AS v, sum(h) h FROM cube_ts GROUP BY 1 ORDER BY h DESC", "init")$v,
    v = q("SELECT DISTINCT v::VARCHAR AS v FROM cube_ts ORDER BY 1", "init")$v
  )
})
META <- jsonlite::fromJSON(q("SELECT json FROM meta", "init")$json)

COUNTRY_NAMES <- c(NO = "Norway", IS = "Iceland", FO = "Faroe Islands", GB = "United Kingdom", IE = "Ireland",
                   FR = "France", ES = "Spain", PT = "Portugal", BE = "Belgium", NL = "Netherlands",
                   DE = "Germany", DK = "Denmark", SE = "Sweden", FI = "Finland", PL = "Poland",
                   EE = "Estonia", LV = "Latvia", LT = "Lithuania")
GEAR_NAMES <- c(OTB = "Bottom otter trawl", TBB = "Beam trawl", OTM = "Midwater otter trawl",
                PTM = "Midwater pair trawl", PS = "Purse seine", SDN = "Danish seine", SSC = "Scottish seine",
                DRB = "Boat dredge", LLS = "Set longline", GNS = "Set gillnet", FPO = "Pots and traps")
named_choices <- function(codes, names) setNames(codes, ifelse(is.na(names[codes]), codes, paste0(codes, " · ", names[codes])))

#' @title Get LakeCat Lake Watershed
#'
#' @description
#' Lookup function for a single COMID from S3 GeoParquet (optionally restricted to one HUC2).
#' Queries one COMID from an S3-hosted, HUC2-partitioned GeoParquet dataset and returns an sf object.
#' If `huc2` is provided, only that partition is scanned (fastest). If not, the function tries
#' a glob over all HUC2 partitions and falls back to a shallower pattern if needed.
#' The function:
#' - loads DuckDB httpfs (S3) extension,
#' - pushes an equality filter on `COMID` for row-group/file pruning,
#' - converts WKB geometry to sf with the CRS you provide (default EPSG:4326).
#'
#' @param comid Scalar COMID to query (numeric or character, required).
#' @param huc2 Optional two-digit HUC2 string (e.g., "01") to restrict search to one partition.
#' @param huc2_filter Optional character vector of HUC2s to read (e.g., c("01","05")) for multi-partition pruning.
#' @param bucket Character(1). S3 bucket (default "dmap-data-commons-ow").
#' @param prefix Character(1). S3 prefix under the bucket (default "data/streamcat/LakeCatWatersheds/").
#' @param region Character(1). S3 region (default "us-east-1").
#' @param install_missing Logical. Install missing packages (duckdb, DBI, sf, wk) if needed (default FALSE).
#' @param keep_open Logical. Keep the DuckDB connection open (default FALSE). Note: the connection is not returned.
#' @param verbose Logical. Print progress messages (default TRUE).
#' @param progress Logical. Show a simple progress bar (default TRUE).
#' @param threads Integer or NULL. If set, `PRAGMA threads` for DuckDB (parallelism).
#' @param enable_object_cache Logical. Enable DuckDB object cache to speed repeated queries (default TRUE).
#' @param skip_describe Logical. Skip DESCRIBE step (default FALSE).
#' @param skip_counts Logical. Skip HUC2 counts step (default TRUE; no longer returned).
#' @param sf_crs Integer or character. CRS for the output sf object (default 4326).
#'
#' @return An sf object with zero or one+ rows (if multiple features share the same COMID).
#' @export

lc_get_watershed <- function(
    comid,
    huc2 = NA_character_,
    huc2_filter = NULL,
    bucket = "dmap-data-commons-ow",
    prefix = "data/streamcat/LakeCatWatersheds/",
    region = "us-east-1",
    install_missing = FALSE,
    keep_open = FALSE,
    verbose = TRUE,
    progress = TRUE,
    threads = 4,                  # default to moderate parallelism
    enable_object_cache = TRUE,
    skip_describe = FALSE,
    skip_counts = TRUE,
    sf_crs = 4326,
    retries = 5,                  # NEW: retry count for transient timeouts
    retry_base_delay = 0.5,       # NEW: initial delay in seconds
    retry_max_delay  = 8,         # NEW: cap delay per attempt
    url_style = c("path", "virtual_hosted"),  # NEW: allows changing URL style
    s3_endpoint = NULL            # NEW: custom endpoint (e.g., "s3.amazonaws.com")
) {
  # --- COMID validation (single-value with warning if vector) ---
  if (missing(comid) || length(comid) == 0L) stop("Argument 'comid' is required.")
  if (length(comid) > 1L) {
    warning("Currently the function only supports requesting one feature at a time by COMID; using the first value: ",
            as.character(comid[1]))
    comid <- comid[1]
  }
  if (is.na(comid)) stop("Argument 'comid' cannot be NA.")
  comid_chr <- as.character(comid)
  
  # --- Packages ---
  needed <- c("duckdb", "DBI", "sf", "wk")
  have <- vapply(needed, requireNamespace, logical(1), quietly = TRUE)
  if (!all(have)) {
    missing <- needed[!have]
    if (isTRUE(install_missing)) {
      install.packages(missing, repos = "https://cloud.r-project.org")
      have <- vapply(needed, requireNamespace, logical(1), quietly = TRUE)
      if (!all(have)) stop("Could not load packages after installation: ", paste(needed[!have], collapse = ", "))
    } else {
      stop("Missing required packages: ", paste(missing, collapse = ", "),
           ". Set install_missing = TRUE to install automatically.")
    }
  }
  
  url_style <- match.arg(url_style)
  msg <- function(...) if (isTRUE(verbose)) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), paste0(...)))
  
  # --- Progress bar (coarse) ---
  total_steps <- 6L + (!skip_describe) + (!skip_counts)
  step <- 0L; pb <- NULL
  bump <- function() { step <<- step + 1L; if (!is.null(pb)) utils::setTxtProgressBar(pb, step) }
  if (isTRUE(progress)) {
    pb <- utils::txtProgressBar(min = 0, max = total_steps, style = 3)
    on.exit(try(close(pb), silent = TRUE), add = TRUE)
  }
  
  msg("duckdb version: ", as.character(utils::packageVersion("duckdb"))); bump()
  
  # --- Connect (single controlled cleanup) ---
  con <- DBI::dbConnect(duckdb::duckdb())
  if (!isTRUE(keep_open)) {
    on.exit({
      valid <- tryCatch(DBI::dbIsValid(con), error = function(e) FALSE)
      if (isTRUE(valid)) suppressWarnings(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE))
    }, add = TRUE)
  }
  if (!is.null(threads)) DBI::dbExecute(con, sprintf("PRAGMA threads=%d;", as.integer(threads)))
  bump()
  
  # --- Configure httpfs/S3 ---
  msg("Loading and configuring httpfs ...")
  DBI::dbExecute(con, "INSTALL httpfs;")
  DBI::dbExecute(con, "LOAD httpfs;")
  DBI::dbExecute(con, sprintf("SET s3_region = '%s';", region))
  DBI::dbExecute(con, "SET s3_use_ssl = true;")
  DBI::dbExecute(con, sprintf("SET s3_url_style = '%s';", if (url_style == "path") "path" else "virtual_hosted"))
  DBI::dbExecute(con, "SET s3_access_key_id = '';")
  DBI::dbExecute(con, "SET s3_secret_access_key = '';")
  DBI::dbExecute(con, "SET s3_session_token = '';")
  if (!is.null(s3_endpoint)) {
    # e.g., "s3.amazonaws.com" or a VPC endpoint; skip silently if not supported
    try(DBI::dbExecute(con, sprintf("SET s3_endpoint = '%s';", s3_endpoint)), silent = TRUE)
  }
  if (isTRUE(enable_object_cache)) {
    DBI::dbExecute(con, "SET enable_object_cache = true;")
  }
  # Optional secret (harmless; helps on some builds)
  try(DBI::dbExecute(con, sprintf("
    CREATE OR REPLACE SECRET s3_public (
      TYPE S3,
      PROVIDER CONFIG,
      KEY_ID '',
      SECRET '',
      REGION '%s'
    );", region)), silent = TRUE)
  bump()
  
  # --- Retry helper for transient HTTP timeouts ---
  sleep_time <- function(k) min(retry_max_delay, retry_base_delay * (2^(k - 1))) * stats::runif(1, 0.9, 1.1)
  db_get_query_retry <- function(sql) {
    for (k in seq_len(retries)) {
      out <- try(DBI::dbGetQuery(con, sql), silent = TRUE)
      if (!inherits(out, "try-error")) return(out)
      emsg <- conditionMessage(attr(out, "condition"))
      # Only retry on network-ish timeouts; otherwise rethrow
      retryable <- grepl("Timeout was reached", emsg, fixed = TRUE) ||
        grepl("Operation timed out",  emsg, fixed = TRUE) ||
        grepl("Temporary failure in name resolution", emsg, fixed = TRUE)
      if (!retryable || k == retries) {
        stop(simpleError(emsg))
      }
      wt <- sleep_time(k)
      msg(sprintf("Transient S3 timeout; retry %d/%d in %.1fs ...", k, retries, wt))
      Sys.sleep(wt)
    }
  }
  
  # --- Build source (glob or restricted partitions) ---
  norm_prefix <- paste0(sub("/+$", "", prefix), "/")
  shallow_glob <- sprintf("s3://%s/%s*/*.parquet",  bucket, norm_prefix)
  deep_glob    <- sprintf("s3://%s/%s**/*.parquet", bucket, norm_prefix)
  
  src_sql <- NULL
  if (length(huc2_filter)) {
    parts <- sprintf("s3://%s/%sHUC2=%s/*.parquet", bucket, norm_prefix, huc2_filter)
    paths_sql <- paste0("[", paste(sprintf("'%s'", parts), collapse = ", "), "]")
    src_sql <- sprintf("read_parquet(%s, hive_partitioning = true)", paths_sql)
  } else {
    pick_glob <- function(globs) {
      for (g in globs) {
        ok <- try({
          db_get_query_retry(sprintf("SELECT 1 FROM read_parquet('%s', hive_partitioning = true) LIMIT 1", g))
          TRUE
        }, silent = TRUE)
        if (isTRUE(ok)) return(g)
      }
      NULL
    }
    glob <- pick_glob(c(deep_glob, shallow_glob))
    if (is.null(glob)) {
      stop("No Parquet files found under s3://", bucket, "/", norm_prefix,
           " (checked patterns: ", deep_glob, " and ", shallow_glob, ").")
    }
    msg("Using glob: ", glob)
    src_sql <- sprintf("read_parquet('%s', hive_partitioning = true)", glob)
  }
  bump()
  
  # --- Optional describe/counts (not returned) ---
  if (!isTRUE(skip_describe)) {
    msg("Describing schema ...")
    invisible(db_get_query_retry(sprintf("DESCRIBE SELECT * FROM %s LIMIT 0", src_sql)))
  }
  bump()
  if (!isTRUE(skip_counts)) {
    msg("Counting rows per HUC2 ...")
    invisible(db_get_query_retry(sprintf("
      SELECT HUC2, COUNT(*) AS n
      FROM %s
      GROUP BY HUC2
      ORDER BY HUC2
    ", src_sql)))
  }
  bump()
  
  # --- COMID query (with retries) ---
  msg("Querying COMID = ", comid_chr, if (!is.na(huc2)) paste0(" within HUC2=", huc2) else "")
  where <- sprintf("CAST(COMID AS VARCHAR) = '%s'", comid_chr)
  if (!is.na(huc2)) where <- sprintf("%s AND HUC2 = '%s'", where, huc2)
  sql <- sprintf("SELECT * FROM %s WHERE %s", src_sql, where)
  res <- db_get_query_retry(sql)
  msg("Rows returned: ", nrow(res))
  bump()
  
  # --- Convert to sf ---
  geom_candidates <- c("geometry", "wkb_geometry", "geom", "wkb")
  geom_col <- intersect(names(res), geom_candidates)
  if (!length(geom_col)) {
    stop("No geometry column found in result; checked: ",
         paste(geom_candidates, collapse = ", "),
         ". Ensure the dataset contains WKB geometry and uses one of these column names.")
  }
  gcol <- geom_col[1]
  gval <- res[[gcol]]
  if (inherits(gval, "blob")) gval <- unclass(gval)
  if (is.raw(gval)) gval <- as.list(gval)
  sfc <- if (length(gval) == 0L) sf::st_sfc(crs = sf_crs) else sf::st_as_sfc(wk::wkb(gval), crs = sf_crs)
  data_no_geom <- res[, setdiff(names(res), gcol), drop = FALSE]
  result_sf <- sf::st_sf(data_no_geom, geometry = sfc) |> 
    sf::st_set_crs(sf_crs)
  
  return(result_sf)
}

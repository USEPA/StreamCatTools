#' @title Get LakeCat Lake Watershed
#'
#' @description
#' Fast lookup for a single COMID from S3 GeoParquet (optionally restricted to one HUC2).
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
    threads = NULL,
    enable_object_cache = TRUE,
    skip_describe = FALSE,
    skip_counts = TRUE,
    sf_crs = 4326
) {
  # -- Require COMID ----------------------------------------------------------
  if (missing(comid) || is.na(comid)) {
    stop("Argument 'comid' is required (numeric or character).")
  }
  comid_chr <- as.character(comid)
  
  # -- Packages ---------------------------------------------------------------
  needed <- c("duckdb", "DBI", "sf", "wk")
  have <- vapply(needed, requireNamespace, logical(1), quietly = TRUE)
  if (!all(have)) {
    missing <- needed[!have]
    if (isTRUE(install_missing)) {
      install.packages(missing, repos = "https://cloud.r-project.org")
      have <- vapply(needed, requireNamespace, logical(1), quietly = TRUE)
      if (!all(have)) {
        stop("Could not load packages after installation: ",
             paste(needed[!have], collapse = ", "))
      }
    } else {
      stop("Missing required packages: ", paste(missing, collapse = ", "),
           ". Set install_missing = TRUE to install automatically.")
    }
  }
  
  msg <- function(...) if (isTRUE(verbose)) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), paste0(...)))
  
  # -- Progress bar -----------------------------------------------------------
  total_steps <- 6L + (!skip_describe) + (!skip_counts)
  step <- 0L; pb <- NULL
  bump <- function() { step <<- step + 1L; if (!is.null(pb)) utils::setTxtProgressBar(pb, step) }
  if (isTRUE(progress)) {
    pb <- utils::txtProgressBar(min = 0, max = total_steps, style = 3)
    on.exit(try(close(pb), silent = TRUE), add = TRUE)
  }
  
  msg("duckdb version: ", as.character(utils::packageVersion("duckdb")))
  bump()
  
  # -- Connect ---------------------------------------------------------------
  con <- DBI::dbConnect(duckdb::duckdb())
  if (!isTRUE(keep_open)) {
    on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  }
  if (!is.null(threads)) {
    DBI::dbExecute(con, sprintf("PRAGMA threads=%d;", as.integer(threads)))
  }
  bump()
  
  # -- Configure httpfs/S3 ---------------------------------------------------
  msg("Loading and configuring httpfs ...")
  DBI::dbExecute(con, "INSTALL httpfs;")
  DBI::dbExecute(con, "LOAD httpfs;")
  DBI::dbExecute(con, sprintf("SET s3_region = '%s';", region))
  DBI::dbExecute(con, "SET s3_use_ssl = true;")
  DBI::dbExecute(con, "SET s3_url_style = 'path';")
  DBI::dbExecute(con, "SET s3_access_key_id = '';")
  DBI::dbExecute(con, "SET s3_secret_access_key = '';")
  DBI::dbExecute(con, "SET s3_session_token = '';")
  if (isTRUE(enable_object_cache)) {
    DBI::dbExecute(con, "SET enable_object_cache = true;")
  }
  try(DBI::dbExecute(con, sprintf("
    CREATE OR REPLACE SECRET s3_public (
      TYPE S3,
      PROVIDER CONFIG,
      KEY_ID '',
      SECRET '',
      REGION '%s'
    );
  ", region)), silent = TRUE)
  bump()
  
  # -- Build source (glob or restricted partitions) --------------------------
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
          DBI::dbGetQuery(con, sprintf("SELECT 1 FROM read_parquet('%s', hive_partitioning = true) LIMIT 1", g))
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
  
  # -- Optional describe/counts (not returned) --------------------------------
  if (!isTRUE(skip_describe)) {
    msg("Describing schema ...")
    invisible(DBI::dbGetQuery(con, sprintf("DESCRIBE SELECT * FROM %s LIMIT 0", src_sql)))
  }
  bump()
  if (!isTRUE(skip_counts)) {
    msg("Counting rows per HUC2 ...")
    invisible(DBI::dbGetQuery(con, sprintf("
      SELECT HUC2, COUNT(*) AS n
      FROM %s
      GROUP BY HUC2
      ORDER BY HUC2
    ", src_sql)))
  }
  bump()
  
  # -- COMID query ------------------------------------------------------------
  msg("Querying COMID = ", comid_chr, if (!is.na(huc2)) paste0(" within HUC2=", huc2) else "")
  where <- sprintf("CAST(COMID AS VARCHAR) = '%s'", comid_chr)
  if (!is.na(huc2)) {
    where <- sprintf("%s AND HUC2 = '%s'", where, huc2)
  }
  sql <- sprintf("SELECT * FROM %s WHERE %s", src_sql, where)
  res <- DBI::dbGetQuery(con, sql)
  msg("Rows returned: ", nrow(res))
  bump()
  
  # -- Convert to sf ----------------------------------------------------------
  # Detect geometry column; try common names
  geom_candidates <- c("geometry", "wkb_geometry", "geom", "wkb")
  geom_col <- intersect(names(res), geom_candidates)
  
  if (!length(geom_col)) {
    stop("No geometry column found in result; checked: ", paste(geom_candidates, collapse = ", "),
         ". Ensure the dataset contains WKB geometry and uses one of these column names.")
  }
  gcol <- geom_col[1]
  
  # Convert WKB to sfc
  gval <- res[[gcol]]
  if (inherits(gval, "blob")) {
    gval <- unclass(gval)  # blob::blob -> list(raw)
  }
  if (is.raw(gval)) {
    gval <- as.list(gval)  # single raw -> list(raw)
  }
  
  if (length(gval) == 0L) {
    sfc <- sf::st_sfc(crs = sf_crs)  # empty sfc with CRS
  } else {
    sfc <- sf::st_as_sfc(wk::wkb(gval), crs = sf_crs)
  }
  
  data_no_geom <- res[, setdiff(names(res), gcol), drop = FALSE]
  result_sf <- sf::st_sf(data_no_geom, geometry = sfc) |> 
    sf::st_set_crs(4326)
  
  # -- Disconnect if requested -------------------------------------------------
  if (!isTRUE(keep_open)) {
    try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
  }
  
  return(result_sf)
}

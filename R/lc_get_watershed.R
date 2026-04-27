#' Fast lookup of a single COMID from S3 GeoParquet (optionally restricted to one HUC2)
#'
#' Queries one COMID from an S3-hosted, HUC2-partitioned GeoParquet dataset and returns an sf object.
#' If \code{huc2} is provided, only that partition is scanned (fastest). If not, the function tries
#' a default set of HUC2 candidates (01..21) until it finds a match, reusing a single DuckDB
#' connection for speed. The function:
#' - loads DuckDB httpfs (S3) and spatial extensions,
#' - selects only geometry + \code{id_col} + optional \code{extras},
#' - pushes the equality filter on \code{id_col} for row-group/file pruning,
#' - never reads the HUC2 column from files (avoids mixed-type issues),
#' - returns geometry as WKB with CRS you provide (default EPSG:4326).
#'
#' @param comid Scalar COMID to query (numeric or character).
#' @param huc2 Optional two-digit HUC2 string (e.g., "01") to restrict search to one partition.
#' @param bucket_root Character(1). S3 prefix to the dataset root (default
#'   "s3://dmap-data-commons-ow/data/streamcat/LakeCatWatersheds").
#' @param extras Character vector of additional attribute columns to return (case-insensitive).
#'   Extras not present in the schema are ignored with a warning.
#' @param id_col Character(1). Name of the ID column in the Parquet files (default "COMID").
#' @param geometry_col Character(1). Name of the geometry column (default "geometry").
#'   Fallbacks "wkb_geometry" or "geom" are auto-detected if "geometry" is absent.
#' @param huc2_candidates Character vector of HUC2s to try if \code{huc2} is NULL (default 01..21).
#' @param anonymous Logical. Use anonymous/public S3 access (default TRUE). Set FALSE to use AWS creds.
#' @param region Character(1). S3 region (default "us-east-1").
#' @param threads Integer or NULL. If set, PRAGMA threads for DuckDB (parallelism).
#' @param crs Integer or character. CRS for the output sf object (default 4326).
#'
#' @return An sf object with zero or one+ rows (if multiple features share the same COMID).
#'         Includes an HUC2 column derived from the partition that matched.
#' @import DBI duckdb sf
#' @export
lc_get_watershed <- function(
    comid,
    huc2 = NULL,
    bucket_root = "s3://dmap-data-commons-ow/data/streamcat/LakeCatWatersheds",
    extras = character(),
    id_col = "COMID",
    geometry_col = "geometry",
    huc2_candidates = sprintf("%02d", 1:21),
    anonymous = TRUE,
    region = "us-east-1",
    threads = NULL,
    crs = 4326
) {
  # Validate COMID is scalar
  if (length(comid) != 1L || is.na(comid)) {
    stop("comid must be a single non-NA value.", call. = FALSE)
  }
  if (!is.null(huc2)) {
    if (!is.character(huc2) || length(huc2) != 1L) {
      stop("huc2 must be a single two-digit character value like '01', or NULL.", call. = FALSE)
    }
  }
  
  # Open and configure DuckDB once
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
  DBI::dbExecute(con, "INSTALL spatial; LOAD spatial;")
  if (!is.null(threads)) {
    DBI::dbExecute(con, sprintf("PRAGMA threads=%d;", as.integer(threads)))
  }
  # Optional cache for repeated S3 object access within the same session
  try(DBI::dbExecute(con, "PRAGMA enable_object_cache=true;"), silent = TRUE)
  
  DBI::dbExecute(con, sprintf("SET s3_region='%s';", region))
  DBI::dbExecute(con, "SET s3_use_ssl=1;")
  DBI::dbExecute(con, "SET s3_url_style='path';")
  if (isTRUE(anonymous)) {
    DBI::dbExecute(con, "SET s3_access_key_id='';")
    DBI::dbExecute(con, "SET s3_secret_access_key='';")
    DBI::dbExecute(con, "SET s3_session_token='';")
  }
  
  # Helper: convert data.frame with WKB to sf
  as_sf_ <- function(df, crs) {
    if (!nrow(df)) return(sf::st_sf(sf::st_sfc(), crs = crs)[, 0])
    geom_wkb <- structure(df$geometry, class = "WKB")
    sfc <- tryCatch(sf::st_as_sfc(geom_wkb, EWKB = TRUE, crs = crs),
                    error = function(e) sf::st_as_sfc(geom_wkb, EWKB = FALSE, crs = crs))
    sf::st_sf(df[setdiff(names(df), "geometry")], geometry = sfc, crs = crs)
  }
  
  # Helper: resolve requested columns against actual schema (case-insensitive)
  resolve_cols_ <- function(con, pattern, id_col, geometry_col, extras) {
    probe <- DBI::dbGetQuery(con, sprintf("SELECT * FROM parquet_scan('%s') LIMIT 0", pattern))
    avail <- names(probe)
    map1 <- function(x) { i <- match(tolower(x), tolower(avail)); if (is.na(i)) NA_character_ else avail[i] }
    id_real   <- map1(id_col)
    geom_real <- map1(geometry_col)
    if (is.na(id_real)) stop(sprintf('id_col "%s" not found. Available: %s', id_col, paste(avail, collapse = ", ")), call. = FALSE)
    if (is.na(geom_real)) {
      fb <- c("wkb_geometry", "geom")
      i <- match(tolower(fb), tolower(avail))
      if (any(!is.na(i))) {
        geom_real <- avail[i[which(!is.na(i))[1]]]
        warning(sprintf('geometry_col not found as "%s". Using "%s".', geometry_col, geom_real), call. = FALSE)
      } else {
        stop(sprintf('geometry_col "%s" not found. Available: %s', geometry_col, paste(avail, collapse = ", ")), call. = FALSE)
      }
    }
    extras_real <- vapply(extras, map1, character(1))
    if (any(is.na(extras_real))) {
      missing_extras <- extras[is.na(extras_real)]
      if (length(missing_extras)) {
        warning(sprintf('Ignoring extras not present: %s', paste(missing_extras, collapse = ", ")), call. = FALSE)
      }
      extras_real <- extras_real[!is.na(extras_real)]
    }
    list(id = id_real, geom = geom_real, extras = unique(extras_real))
  }
  
  # Helper: run the fast query on a single HUC2 using the existing connection
  query_one_huc2_ <- function(huc2_val) {
    pattern <- sprintf("%s/HUC2=%s/*.parquet", bucket_root, huc2_val)
    cols <- resolve_cols_(con, pattern, id_col, geometry_col, extras)
    
    # Build minimal SELECT with WKB geometry
    geom_expr <- sprintf('ST_AsWKB("%s") AS geometry', cols$geom)
    other_cols <- unique(c(cols$id, cols$extras))
    other_expr <- if (length(other_cols)) paste(sprintf('"%s"', other_cols), collapse = ", ") else ""
    select_list <- paste(c(geom_expr, other_expr), collapse = if (nzchar(other_expr)) ", " else "")
    
    # Literal for COMID
    value_sql <- if (is.numeric(comid)) as.character(comid) else DBI::dbQuoteString(con, as.character(comid))
    
    sql <- sprintf(
      "SELECT %s
       FROM parquet_scan('%s')
       WHERE \"%s\" = %s
       LIMIT 1",
      select_list, pattern, cols$id, value_sql
    )
    
    df <- DBI::dbGetQuery(con, sql)
    if (!nrow(df)) return(NULL)
    
    g <- as_sf_(df, crs = crs)
    g$HUC2 <- huc2_val
    g
  }
  
  # Known HUC2: single fast query
  if (!is.null(huc2)) {
    res <- tryCatch(query_one_huc2_(huc2), error = function(e) NULL)
    if (is.null(res)) {
      return(sf::st_sf(sf::st_sfc(), crs = crs)[, 0])
    }
    return(res)
  }
  
  # Unknown HUC2: reuse the same connection and try candidates in order
  for (h in huc2_candidates) {
    res <- tryCatch(query_one_huc2_((h)), error = function(e) NULL)
    if (!is.null(res) && nrow(res)) return(res)
  }
  
  # Not found
  sf::st_sf(sf::st_sfc(), crs = crs)[, 0]
}

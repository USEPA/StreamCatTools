#' @title Get COMIDs via ArcGIS batch query
#'
#' @description
#' Retrieve NHDPlus catchment FEATUREID (COMID) for a set of point locations
#' by batching points to an ArcGIS Feature Service /query endpoint and mapping
#' the returned catchment geometries back to the input points via a local
#' spatial intersection. This avoids per-point HTTP requests and is faster for
#' moderate-to-large point sets.
#'
#' @param points_sf An sf POINT object containing the input locations. If you
#' supply a plain data.frame, convert it to an sf object first.
#' @param service_layer_url URL to the ArcGIS layer /query endpoint or the
#' layer resource. Defaults to the EPA NHDPlus NP21 simplified catchments
#' layer (MapServer/0).
#' @param layer_id Deprecated. Left for compatibility if you need to append a
#' layer id to the base service URL.
#' @param chunk_size Number of points to send per batch request (default 1000).
#' @param feature_id_field Name of the feature attribute to return as the COMID
#' (default "FEATUREID").
#' @param verbose Logical, print progress messages for each chunk (default TRUE).
#'
#' @return An integer vector of FEATUREID (COMID) values in the same order as
#' the input points. Missing values are returned as NA.
#'
#' @examples
#' \dontrun{
#' pts <- sf::st_as_sf(data.frame(id = 1:3,
#'   x = c(-90, -90.1, -90.2), y = c(40, 40.1, 40.2)), coords = c('x', 'y'), crs = 4326)
#'
#' system.time({
#'   comids <- sc_get_comid(
#'     points_sf = pts,
#'     chunk_size = 500
#'   )
#' })
#' }
#'
#' @export
sc_get_comid <- function(points_sf,
                         service_layer_url = "https://watersgeo.epa.gov/arcgis/rest/services/NHDPlus_NP21/Catchments_NP21_Simplified/MapServer/0",
                         layer_id = NULL,
                         chunk_size = 1000,
                         feature_id_field = "FEATUREID",
                         verbose = TRUE,
                         crs = NULL,
                         coords = NULL) {
  if (inherits(points_sf, "sf")) {
    pts <- points_sf
    if (!is.null(crs)) {
      pts_crs <- sf::st_crs(pts)
      if (is.na(pts_crs)) {
        pts <- sf::st_set_crs(pts, crs)
      }
    }
    if (is.na(sf::st_crs(pts))) {
      stop("points_sf is an sf object with no CRS. Supply crs = ... or provide an sf object with an assigned CRS.")
    }
  } else if (is.data.frame(points_sf)) {
    if (is.null(coords)) {
      x_name <- NULL
      y_name <- NULL
      if (all(c("x", "y") %in% names(points_sf))) {
        x_name <- "x"
        y_name <- "y"
      } else if (all(c("lon", "lat") %in% names(points_sf))) {
        x_name <- "lon"
        y_name <- "lat"
      } else if (all(c("longitude", "latitude") %in% names(points_sf))) {
        x_name <- "longitude"
        y_name <- "latitude"
      } else if (all(c("LON_SITE", "LAT_SITE") %in% names(points_sf))) {
        x_name <- "LON_SITE"
        y_name <- "LAT_SITE"
      } else {
        stop("points_sf must be an sf object or a data.frame with x/y or lon/lat coordinate columns; also pass coords = c('x', 'y') if needed.")
      }
    } else {
      if (!is.character(coords) || length(coords) != 2L) {
        stop("coords must be a character vector of length 2 of x/y coordinate names.")
      }
      x_name <- coords[1L]
      y_name <- coords[2L]
      if (!(x_name %in% names(points_sf)) || !(y_name %in% names(points_sf))) {
        stop("The coordinate columns specified in coords are not present in points_sf.")
      }
    }

    if (is.null(crs)) crs <- 4326
    pts <- sf::st_as_sf(points_sf, coords = c(x_name, y_name), crs = crs)
  } else {
    stop("points_sf must be an sf POINT object or a data.frame with coordinate columns.")
  }

  # normalize URL to the layer's /query endpoint
  if (!grepl("/query$", service_layer_url)) {
    service_layer_url <- sub("/+$", "", service_layer_url)
    service_layer_url <- paste0(service_layer_url, "/query")
  }

  # get layer metadata to detect spatial reference
  meta_url <- sub("/query$", "", service_layer_url)
  meta <- tryCatch({
    httr2::request(meta_url) |> httr2::req_url_query(f = "json") |> httr2::req_perform() |> httr2::resp_body_json(simplifyVector = TRUE)
  }, error = function(e) NULL)
  sr_wkid <- NULL
  if (!is.null(meta)) {
    if (!is.null(meta$spatialReference$wkid)) sr_wkid <- meta$spatialReference$wkid
    if (is.null(sr_wkid) && !is.null(meta$extent$spatialReference$wkid)) sr_wkid <- meta$extent$spatialReference$wkid
  }
  if (is.null(sr_wkid)) sr_wkid <- 4326L

  # normalize ESRI web‑mercator wkids
  if (!is.null(sr_wkid) && sr_wkid %in% c(102100L, 102113L)) sr_wkid <- 3857L

  # prepare points transformed to service CRS
  pts <- sf::st_transform(pts, crs = sr_wkid)
  pts$.pt_row_internal <- seq_len(nrow(pts))
  coords <- sf::st_coordinates(pts)[, c("X", "Y"), drop = FALSE]
  if (nrow(coords) == 0L) return(integer(0))
  idx <- split(seq_len(nrow(coords)), ceiling(seq_len(nrow(coords)) / chunk_size))
  out_list <- vector("list", length(idx))

  for (i in seq_along(idx)) {
    if (verbose) cat(sprintf("chunk %d/%d: %d points\n", i, length(idx), length(idx[[i]])))
    chunk_coords <- coords[idx[[i]], , drop = FALSE]
    pts_list <- lapply(seq_len(nrow(chunk_coords)), function(j) c(as.numeric(chunk_coords[j,1]), as.numeric(chunk_coords[j,2])))
    geom <- list(points = pts_list)
    geom_json <- jsonlite::toJSON(geom, auto_unbox = TRUE)

    query <- list(
      f = "json",
      geometry = geom_json,
      geometryType = "esriGeometryMultipoint",
      inSR = sr_wkid,
      spatialRel = "esriSpatialRelIntersects",
      outFields = "*",
      returnGeometry = "true",
      outSR = sr_wkid
    )

    resp <- tryCatch({
      httr2::request(service_layer_url) |> httr2::req_url_query(!!!query) |> httr2::req_perform() |> httr2::resp_body_json(simplifyVector = FALSE)
    }, error = function(e) {
      warning(sprintf("ArcGIS request failed for chunk %d: %s", i, e$message))
      return(NULL)
    })

    if (is.null(resp) || is.null(resp$features) || length(resp$features) == 0) {
      out_list[[i]] <- tibble::tibble(.pt_row_internal = idx[[i]], FEATUREID = NA_integer_)
      next
    }

    feats <- resp$features
    attrs <- lapply(feats, function(f) as.data.frame(f$attributes, stringsAsFactors = FALSE)) |> dplyr::bind_rows()

    geoms <- lapply(feats, function(f) {
      g <- f$geometry
      if (!is.null(g$rings)) {
        rings <- lapply(g$rings, function(r) {
          do.call(rbind, lapply(r, function(x) c(as.numeric(x[1]), as.numeric(x[2]))))
        })
        sf::st_polygon(rings)
      } else if (!is.null(g$x) && !is.null(g$y)) {
        sf::st_point(c(as.numeric(g$x), as.numeric(g$y)))
      } else {
        sf::st_geometrycollection()
      }
    })

    geom_sfc <- sf::st_sfc(lapply(geoms, function(g) if (inherits(g, "sfg")) g else sf::st_geometrycollection()), crs = sr_wkid)
    polys_sf <- sf::st_sf(attrs, geometry = geom_sfc)

    chunk_pts <- pts[idx[[i]], , drop = FALSE]
    joined <- sf::st_intersects(chunk_pts, polys_sf)
    match_feat <- vapply(joined, function(x) if (length(x)) x[1] else NA_integer_, integer(1))
    matched_featureid <- if (nrow(polys_sf) > 0 && feature_id_field %in% names(polys_sf)) polys_sf[[feature_id_field]][match_feat] else rep(NA_integer_, length(match_feat))

    out_list[[i]] <- tibble::tibble(.pt_row_internal = chunk_pts$.pt_row_internal, FEATUREID = matched_featureid)
  }

  out_df <- dplyr::bind_rows(out_list) |> dplyr::arrange(.pt_row_internal)
  out_df$FEATUREID
}

.sc_lake_with_connection <- function(query) {
  root <- getOption(
    "StreamCatTools.lake_root",
    "s3://dmap-data-commons-ow/data/streamcat/lake_full"
  )
  if (
    !is.character(root) || length(root) != 1L || is.na(root) || !nzchar(root)
  ) {
    stop(
      "StreamCatTools.lake_root must be a nonempty path or URL.",
      call. = FALSE
    )
  }

  remote <- grepl("^(s3|https?|gs|r2)://", root)
  if (!remote) {
    root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  }
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  if (remote) {
    DBI::dbExecute(con, "INSTALL httpfs")
    DBI::dbExecute(con, "LOAD httpfs")
    if (startsWith(root, "s3://")) {
      DBI::dbExecute(con, "SET s3_url_style = 'path'")
    }
  }
  query(con, sub("/+$", "", root))
}

.sc_lake_parquet <- function(con, root, path) {
  uri <- as.character(DBI::dbQuoteString(con, paste0(root, "/", path)))
  paste0("read_parquet(", uri, ", hive_partitioning = false)")
}

.sc_lake_tokens <- function(x) {
  x <- as.character(unlist(x, use.names = FALSE))
  unique(trimws(unlist(strsplit(x, ",", fixed = TRUE), use.names = FALSE)))
}

.sc_lake_aois <- function(x) {
  x <- tolower(x)
  aliases <- c(
    catchment = "cat",
    watershed = "ws",
    riparian_catchment = "catrp100",
    riparian_watershed = "wsrp100"
  )
  alias <- x %in% names(aliases)
  x[alias] <- aliases[x[alias]]
  unique(x)
}

.sc_lake_catalog <- function(con, root) {
  catalog <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT * FROM",
      .sc_lake_parquet(con, root, "metric_catalog.parquet")
    )
  )
  catalog$aoi <- tolower(catalog$aoi)
  catalog$aoi[is.na(catalog$aoi) | catalog$aoi == ""] <- "other"

  stored <- tolower(catalog$metric)
  # Riparian coverage is dataset context, not a standalone legacy metric.
  keep <- !stored %in%
    c("catpctfull", "wspctfull", "catpctfullrp100", "wspctfullrp100")
  catalog <- catalog[keep, , drop = FALSE]
  stored <- stored[keep]
  suffix <- ifelse(catalog$aoi == "other", "", catalog$aoi)
  has_suffix <- nzchar(suffix) & endsWith(stored, suffix)
  prefix <- stored
  prefix[has_suffix] <- substr(
    stored[has_suffix],
    1L,
    nchar(stored[has_suffix]) - nchar(suffix[has_suffix])
  )

  year <- ifelse(is.na(catalog$year), "", as.character(catalog$year))
  wide <- nzchar(year) &
    (catalog$year_in_name %in% TRUE | endsWith(prefix, year))
  catalog$token <- paste0(prefix, ifelse(wide, "", year))
  catalog$base_token <- tolower(catalog$base_metric)
  catalog$year_token <- paste0(catalog$base_token, year)

  # The width/depth parquet stores units in the physical column names.
  channel_names <- c(
    bankfull_depth_m = "bankfulldepth",
    bankfull_width_m = "bankfullwidth",
    thalweg_depth_cm = "thalwagdepth",
    wetted_width_m = "wettedwidth"
  )
  channel <- catalog$token %in% names(channel_names)
  catalog$token[channel] <- channel_names[catalog$token[channel]]
  catalog$out_name <- paste0(catalog$token, suffix)

  for (col in c("overlay_base_path", "overlay_base_metric")) {
    if (!col %in% names(catalog)) {
      catalog[[col]] <- rep(NA_character_, nrow(catalog))
    }
  }
  catalog
}

.sc_lake_resolve_collisions <- function(rows) {
  if (!anyDuplicated(rows$out_name)) {
    return(rows)
  }

  # The legacy NNI time series includes 1987; the one-year exports duplicate it.
  # Prefer the full time series, not whichever row happens to be returned first.
  duplicate_names <- unique(rows$out_name[duplicated(rows$out_name)])
  for (name in duplicate_names) {
    i <- which(rows$out_name == name)
    canonical <- grepl("^[np]_leg_(hy_)?1987(cat|ws)$", name) &
      (rows$path[i] %in%
        c(
          "data/dataset=Legacy_Agricultural_Surplus/part-0.parquet",
          "data/dataset=Legacy_Agricultural_Surplus_Hydrologic_Modeling/part-0.parquet"
        ))
    if (sum(canonical) != 1L) {
      stop(
        "Ambiguous StreamCat lake metric column '",
        name,
        "' in: ",
        paste(unique(rows$path[i]), collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    rows <- rows[-i[!canonical], , drop = FALSE]
  }
  rows
}

.sc_lake_select <- function(catalog, metric, aoi) {
  by_aoi <- catalog[catalog$aoi %in% aoi, , drop = FALSE]
  matches <- lapply(metric, function(name) {
    i <- which(by_aoi$token == name | by_aoi$out_name == name)
    if (!length(i)) {
      i <- which(by_aoi$year_token == name | by_aoi$base_token == name)
    }
    i
  })
  missing <- metric[lengths(matches) == 0L]
  if (length(missing)) {
    stop(
      "Unknown or unavailable StreamCat lake metric(s) for the requested AOI(s): ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  cols <- c(
    "metric",
    "path",
    "out_name",
    "overlay_base_path",
    "overlay_base_metric"
  )
  rows <- unique(by_aoi[unique(unlist(matches)), cols, drop = FALSE])
  .sc_lake_resolve_collisions(rows)
}

.sc_lake_comids <- function(comid) {
  comid <- unlist(comid, use.names = FALSE)
  if (is.numeric(comid)) {
    comid <- format(comid, scientific = FALSE, trim = TRUE)
  }
  ids <- .sc_lake_tokens(comid)
  id <- suppressWarnings(as.integer(ids))
  if (
    !length(ids) || anyNA(id) || any(!grepl("^[0-9]+$", ids)) || any(id <= 0L)
  ) {
    stop("COMIDs must be positive 32-bit integers.", call. = FALSE)
  }
  sort(unique(id))
}

.sc_lake_comid_filter <- function(ids) {
  if (!length(ids)) {
    return(character())
  }
  # Bounds are explicit so DuckDB can prune sorted Parquet row groups even
  # when the exact IN predicate is implemented as a join.
  paste0(
    "COMID BETWEEN ",
    min(ids),
    " AND ",
    max(ids),
    " AND COMID IN (",
    paste(ids, collapse = ", "),
    ")"
  )
}

.sc_lake_where <- function(predicates) {
  if (length(predicates)) {
    paste("WHERE", paste(predicates, collapse = " AND "))
  } else {
    ""
  }
}

.sc_lake_partition <- function(con, root, rows, comid_filter) {
  ident <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  scan <- function(path) {
    paste(
      "(SELECT * FROM",
      .sc_lake_parquet(con, root, path),
      .sc_lake_where(comid_filter),
      ")"
    )
  }
  value <- paste0("o.", ident(rows$metric))
  source <- paste(scan(rows$path[[1L]]), "o")
  base_path <- rows$overlay_base_path[[1L]]
  if (!is.na(base_path)) {
    value <- paste0(
      "COALESCE(",
      value,
      ", b.",
      ident(rows$overlay_base_metric),
      ")"
    )
    source <- paste(
      source,
      "FULL OUTER JOIN",
      scan(base_path),
      "b USING (COMID)"
    )
  }
  projection <- paste(value, "AS", ident(rows$out_name), collapse = ", ")
  paste0("(SELECT COMID, ", projection, " FROM ", source, ")")
}

.sc_lake_area <- c(
  cat = "CatAreaSqKm",
  ws = "WsAreaSqKm",
  catrp100 = "CatAreaSqKmRp100",
  wsrp100 = "WsAreaSqKmRp100"
)

.sc_lake_data <- function(
  comid,
  metric,
  aoi,
  state,
  region,
  conus,
  showAreaSqKm
) {
  metric <- tolower(.sc_lake_tokens(metric))
  aoi <- .sc_lake_aois(.sc_lake_tokens(aoi))
  if (
    !length(metric) || anyNA(metric) || any(!nzchar(metric)) || !length(aoi)
  ) {
    stop("Supply valid metric and aoi values.", call. = FALSE)
  }
  if (!all(aoi %in% c(names(.sc_lake_area), "other"))) {
    stop(
      "Unknown StreamCat AOI(s): ",
      paste(setdiff(aoi, c(names(.sc_lake_area), "other")), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  ids <- integer()
  geography <- NULL
  values <- character()
  if (!.sc_true_flag(conus)) {
    if (!is.null(region)) {
      geography <- "RegionId"
      values <- .sc_lake_tokens(region)
    } else if (!is.null(state)) {
      geography <- "State"
      values <- .sc_lake_tokens(state)
    } else if (!is.null(comid)) {
      ids <- .sc_lake_comids(comid)
    } else {
      stop("Supply comid, state, region, or conus = TRUE.", call. = FALSE)
    }
  }
  if (
    !is.null(geography) &&
      (!length(values) || anyNA(values) || any(!nzchar(values)))
  ) {
    stop("Supply valid ", tolower(geography), " values.", call. = FALSE)
  }
  area <- if (.sc_true_flag(showAreaSqKm)) {
    unname(.sc_lake_area[intersect(aoi, names(.sc_lake_area))])
  }
  comid_filter <- .sc_lake_comid_filter(ids)

  .sc_lake_with_connection(function(con, root) {
    rows <- .sc_lake_select(.sc_lake_catalog(con, root), metric, aoi)
    ident <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
    filters <- comid_filter
    if (!is.null(geography)) {
      quoted <- paste(
        as.character(DBI::dbQuoteString(con, values)),
        collapse = ", "
      )
      filters <- c(filters, paste0(geography, " IN (", quoted, ")"))
    }
    index <- .sc_lake_parquet(con, root, "comid_index.parquet")
    area_sql <- if (length(area)) {
      paste0(
        ident(paste0(area, "_x10000")),
        " / 10000.0 AS ",
        ident(tolower(area))
      )
    }
    driver <- paste0(
      "(SELECT ",
      paste(c("COMID", area_sql), collapse = ", "),
      " FROM ",
      index,
      " ",
      .sc_lake_where(filters),
      ") d"
    )

    # Group by exact file and overlay base: a single file can supply many columns.
    overlay <- !is.na(rows$overlay_base_path) & !is.na(rows$overlay_base_metric)
    rows$overlay_base_path[!overlay] <- NA_character_
    keys <- paste(rows$path, rows$overlay_base_path, sep = "\n")
    groups <- match(keys, unique(keys))
    parts <- lapply(split(rows, groups), function(g) {
      .sc_lake_partition(con, root, g, comid_filter)
    })
    joins <- paste(
      "LEFT JOIN",
      unlist(parts),
      paste0("m", seq_along(parts)),
      "USING (COMID)"
    )
    sql <- paste(
      "SELECT",
      paste(
        c("d.COMID AS comid", ident(c(tolower(area), rows$out_name))),
        collapse = ", "
      ),
      "FROM",
      driver,
      paste(joins, collapse = " "),
      "ORDER BY d.COMID"
    )
    result <- DBI::dbGetQuery(con, sql)
    keep <- names(result) %in%
      c("comid", tolower(area)) |
      !vapply(result, \(x) all(is.na(x)), logical(1))
    result[, keep, drop = FALSE]
  })
}

.sc_lake_params <- function(param) {
  if (length(param) != 1L || !param %in% c("metric_names", "region", "aoi")) {
    stop(
      "Unsupported StreamCat lake parameter: ",
      paste(param, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  .sc_lake_with_connection(function(con, root) {
    if (param == "region") {
      index <- .sc_lake_parquet(con, root, "comid_index.parquet")
      sql <- paste(
        "SELECT DISTINCT RegionId FROM",
        index,
        "WHERE RegionId IS NOT NULL ORDER BY 1"
      )
      return(DBI::dbGetQuery(con, sql)[[1L]])
    }
    catalog <- .sc_lake_catalog(con, root)
    sort(unique(if (param == "aoi") catalog$aoi else catalog$token))
  })
}

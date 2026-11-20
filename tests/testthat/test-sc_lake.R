make_sc_lake_fixture <- function(root) {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  write_parquet <- function(rows, path, types = character()) {
    dest <- file.path(root, path)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    DBI::dbWriteTable(con, "fixture_rows", rows, overwrite = TRUE)
    cols <- names(rows)
    quoted <- DBI::dbQuoteIdentifier(con, cols)
    select <- as.character(quoted)
    cast <- cols %in% names(types)
    select[cast] <- sprintf(
      "CAST(%s AS %s) AS %s",
      quoted[cast],
      types[cols[cast]],
      quoted[cast]
    )
    DBI::dbExecute(
      con,
      sprintf(
        "COPY (SELECT %s FROM fixture_rows) TO %s (FORMAT PARQUET)",
        paste(select, collapse = ", "),
        DBI::dbQuoteString(con, dest)
      )
    )
  }

  ids <- 101:104
  index <- data.frame(
    COMID = ids,
    RegionId = c("Region05", "Region05", "Region06", NA),
    State = c("KY", "OH", "KY", NA),
    StateFips = c(21L, 39L, 21L, NA_integer_),
    CatAreaSqKm_x10000 = c(12345L, 25678L, 32000L, NA_integer_),
    WsAreaSqKm_x10000 = c(987654L, 245678L, 456789L, NA_integer_),
    CatAreaSqKmRp100_x10000 = c(2345L, 5678L, 2000L, NA_integer_),
    WsAreaSqKmRp100_x10000 = c(87654L, 45678L, 56789L, NA_integer_)
  )
  index_types <- c(
    StateFips = "UTINYINT",
    stats::setNames(
      rep("BIGINT", 4),
      tail(names(index), 4)
    )
  )
  write_parquet(index, "comid_index.parquet", index_types)

  catalog_rows <- function(
    rows,
    dataset,
    base_metric,
    aoi,
    stored_type,
    source_file,
    year = NA_integer_,
    year_in_name = FALSE,
    part = 0L,
    overlay_base_path = NA_character_,
    overlay_base_metric = NA_character_
  ) {
    path <- paste0(
      "data/dataset=",
      dataset,
      if (!is.na(year) && !year_in_name) paste0("/year=", year),
      "/part-",
      part,
      ".parquet"
    )
    metric <- setdiff(names(rows), c("COMID", "CatPctFull", "WsPctFull"))
    write_parquet(
      rows,
      path,
      c(
        stats::setNames(stored_type, metric),
        CatPctFull = "DECIMAL(5,2)",
        WsPctFull = "DECIMAL(5,2)"
      )
    )
    data.frame(
      metric,
      base_metric,
      aoi,
      dataset,
      year,
      year_in_name,
      stored_type,
      sig_digits = NA_integer_,
      min_value = vapply(rows[metric], min, numeric(1), na.rm = TRUE),
      max_value = vapply(rows[metric], max, numeric(1), na.rm = TRUE),
      path,
      source_file,
      overlay_base_path,
      overlay_base_metric,
      row.names = NULL
    )
  }

  nlcd <- function(year, rows) {
    catalog_rows(
      rows,
      "NLCD",
      "PctCrop",
      c("Cat", "Ws"),
      c("DECIMAL(5,2)", "DECIMAL(6,3)"),
      paste0("NLCD", year, ".parquet"),
      year
    )
  }
  nlcd_2001 <- nlcd(
    2001L,
    data.frame(
      COMID = ids,
      PctCropCat = c(12.34, 20, 30, 40),
      PctCropWs = c(14.321, 25, 35, 45),
      CatPctFull = c(98, 97, 96, 95),
      WsPctFull = c(94, 93, 92, 91)
    )
  )
  nlcd_2006 <- nlcd(
    2006L,
    data.frame(
      COMID = ids[2:3],
      PctCropCat = c(50, 60),
      PctCropWs = c(55.432, 65),
      CatPctFull = c(88, 87),
      WsPctFull = c(86, 85)
    )
  )
  riparian <- catalog_rows(
    data.frame(
      COMID = ids,
      PctCropCatRp100 = c(1, 2, 3, 4),
      PctCropWsRp100 = c(5, 6, 7, 8)
    ),
    "NLCDRipBuf100",
    "PctCrop",
    c("CatRp100", "WsRp100"),
    c("DECIMAL(5,2)", "DECIMAL(6,3)"),
    "NLCD2001RipBuf100.parquet",
    2001L
  )
  ici <- catalog_rows(
    data.frame(COMID = ids, ICI = c(0.35, 0.45, 0.55, 0.65)),
    "ICI",
    "ICI",
    NA_character_,
    "DECIMAL(6,4)",
    "ICI.parquet"
  )
  base <- catalog_rows(
    data.frame(COMID = ids, n_ags_2017Ws = c(10, 20, 30, 40)),
    "Agricultural_Surplus",
    "n_ags",
    "Ws",
    "DECIMAL(7,4)",
    "Agricultural Surplus.parquet",
    2017L,
    year_in_name = TRUE,
    part = 1L
  )
  overlay <- catalog_rows(
    data.frame(COMID = ids, n_ags_hy_2017Ws = c(NA, 0, NA, NA)),
    "Agricultural_Surplus_Hydrologic_Modeling",
    "n_ags_hy",
    "Ws",
    "DECIMAL(7,4)",
    "Agricultural Surplus Hydrologic Modeling.parquet",
    2017L,
    year_in_name = TRUE,
    overlay_base_path = base$path,
    overlay_base_metric = base$metric
  )
  overlay$min_value <- 0
  overlay$max_value <- 40
  write_parquet(
    rbind(nlcd_2006, ici, overlay, nlcd_2001, riparian, base),
    "metric_catalog.parquet"
  )
}

with_sc_lake_fixture <- function(code) {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  root <- tempfile("streamcat-lake-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  make_sc_lake_fixture(root)
  old <- options(StreamCatTools.lake_root = root, StreamCatTools.backend = NULL)
  on.exit(options(old), add = TRUE, after = FALSE)

  # Fail locally rather than contacting EPA if dispatch falls back to the API.
  network_disabled <- function(...) stop("Network disabled in local lake tests")
  from_json <- jsonlite::fromJSON
  testthat::local_mocked_bindings(
    req_perform = network_disabled,
    .package = "httr2",
    .env = environment()
  )
  testthat::local_mocked_bindings(
    fromJSON = function(txt, ...) {
      if (is.character(txt) && length(txt) == 1L && grepl("^https?://", txt)) {
        network_disabled()
      }
      from_json(txt, ...)
    },
    .package = "jsonlite",
    .env = environment()
  )
  force(code)
}

test_that("lake data uses API names and joins years without duplicate rows or columns", {
  with_sc_lake_fixture({
    out <- sc_get_data(
      comid = "102,101,102",
      metric = c("PctCrop2001", "PCTCROP2006", "pctcrop2001"),
      aoi = "catchment,ws,cat"
    )

    expect_s3_class(out, "data.frame")
    expect_identical(names(out)[1], "comid")
    expect_setequal(
      names(out)[-1],
      c(
        "pctcrop2001cat",
        "pctcrop2001ws",
        "pctcrop2006cat",
        "pctcrop2006ws"
      )
    )
    expect_identical(anyDuplicated(names(out)), 0L)
    expect_equal(sort(out$comid), c(101, 102))
    out <- out[match(c(101, 102), out$comid), ]
    expect_equal(out$pctcrop2001cat, c(12.34, 20))
    expect_equal(out$pctcrop2001ws, c(14.321, 25))
    expect_equal(out$pctcrop2006cat, c(NA, 50))
    expect_equal(out$pctcrop2006ws, c(NA, 55.432))
  })
})

test_that("lake geography honors region, state and CONUS precedence", {
  with_sc_lake_fixture({
    by_region <- sc_get_data(
      comid = 103,
      state = "KY",
      county = "99999",
      region = "Region05",
      metric = "pctcrop2001",
      aoi = "cat"
    )
    expect_equal(sort(by_region$comid), c(101, 102))
    expect_named(by_region, c("comid", "pctcrop2001cat"))

    by_state <- sc_get_data(
      comid = 102,
      state = "KY",
      county = "99999",
      metric = "pctcrop2001",
      aoi = "cat"
    )
    expect_equal(sort(by_state$comid), c(101, 103))

    conus <- sc_get_data(
      comid = 101,
      region = "Region05",
      conus = TRUE,
      metric = "pctcrop2001",
      aoi = "cat"
    )
    expect_equal(sort(conus$comid), 101:104)
  })
})

test_that("lake data decodes optional numeric areas and recognizes riparian AOIs", {
  with_sc_lake_fixture({
    out <- sc_get_data(
      comid = 101,
      metric = "pctcrop2001",
      aoi = c("cat", "watershed"),
      showAreaSqKm = TRUE
    )
    expect_setequal(
      names(out),
      c(
        "comid",
        "pctcrop2001cat",
        "pctcrop2001ws",
        "catareasqkm",
        "wsareasqkm"
      )
    )
    expect_equal(out$catareasqkm, 1.2345)
    expect_equal(out$wsareasqkm, 98.7654)

    riparian <- sc_get_data(
      comid = 101,
      metric = "pctcrop2001",
      aoi = c("riparian_catchment", "wsrp100"),
      showAreaSqKm = "true"
    )
    expect_setequal(
      names(riparian),
      c(
        "comid",
        "pctcrop2001catrp100",
        "pctcrop2001wsrp100",
        "catareasqkmrp100",
        "wsareasqkmrp100"
      )
    )
    expect_equal(riparian$pctcrop2001catrp100, 1)
    expect_equal(riparian$pctcrop2001wsrp100, 5)
    expect_equal(riparian$catareasqkmrp100, 0.2345)
    expect_equal(riparian$wsareasqkmrp100, 8.7654)
  })
})

test_that("lake data reads suffixless metrics and reconstructs wide overlays", {
  with_sc_lake_fixture({
    ici <- sc_get_data(comid = 101, metric = "ICI", aoi = "other")
    expect_named(ici, c("comid", "ici"))
    expect_equal(ici$ici, 0.35)

    out <- sc_get_data(
      comid = c(101, 102, 103),
      metric = "n_ags_2017,n_ags_hy_2017",
      aoi = "ws"
    )
    expect_setequal(names(out), c("comid", "n_ags_2017ws", "n_ags_hy_2017ws"))
    out <- out[match(c(101, 102, 103), out$comid), ]
    expect_equal(out$n_ags_2017ws, c(10, 20, 30))
    expect_equal(out$n_ags_hy_2017ws, c(10, 0, 30))
  })
})

test_that("lake parameters expose public AOIs, metric names and unique regions", {
  with_sc_lake_fixture({
    expect_identical(
      sc_get_params("aoi"),
      c("cat", "catrp100", "other", "ws", "wsrp100")
    )
    expect_identical(sc_get_params("region"), c("Region05", "Region06"))
    expect_identical(
      sc_get_params("metric_names"),
      c(
        "ici",
        "n_ags_2017",
        "n_ags_hy_2017",
        "pctcrop2001",
        "pctcrop2006"
      )
    )
  })
})

test_that("the opt-in remote lake serves a single StreamCat metric", {
  root <- Sys.getenv("STREAMCATTOOLS_TEST_REMOTE_LAKE")
  skip_if(
    !nzchar(root),
    "Set STREAMCATTOOLS_TEST_REMOTE_LAKE to a remote lake URL"
  )
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")

  old <- options(StreamCatTools.lake_root = root, StreamCatTools.backend = NULL)
  out <- tryCatch(
    sc_get_data(comid = 718276, metric = "bfi", aoi = "ws"),
    finally = options(old)
  )
  expect_named(out, c("comid", "bfiws"))
  expect_equal(out$comid, 718276)
  expect_identical(is.finite(out$bfiws), TRUE)
})

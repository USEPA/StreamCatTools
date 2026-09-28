context("Test that lc_get_params is pulling in StreamCat API parameters")

skip_if_api_unavailable <- function(x, label = "API") {
  if (is.null(x) ||
      (is.data.frame(x) && nrow(x) == 0L) ||
      (is.list(x) && length(x) == 0L) ||
      (is.character(x) && length(x) == 0L)) {
    testthat::skip(paste(label, "is unavailable; upstream service returned no data."))
  }
  invisible(x)
}

test_that("lc_get_params for metric_names parameters", {
  testthat::skip_on_cran()
  params <- lc_get_params(param='metric_names')
  skip_if_api_unavailable(params, "LakeCat metric_names API")
  expect_true(exists("params"))
  expect_gt(length(params),500)
})

test_that("lc_get_params for variable_info parameters", {
  testthat::skip_on_cran()
  params <- lc_get_params(param='variable_info')
  skip_if_api_unavailable(params, "LakeCat variable_info API")
  expect_true(exists("params"))
  expect_gt(nrow(params),100)
})

test_that("lc_get_metric_names", {
  testthat::skip_on_cran()
  metrics <- lc_get_metric_names(category='Natural')
  skip_if_api_unavailable(metrics, "LakeCat metric names API")
  expect_true(exists("metrics"))
  expect_gt(nrow(metrics),20)
  expect_equal(names(metrics), c("Category","Metric","AOI","Year", 
                                 "Short_Name","Metric_Description","Units",
                                 "Source","Dataset"))
})

test_that("lc_get_params for area of interest parameters", {
  testthat::skip_on_cran()
  params <- lc_get_params(param='aoi')
  skip_if_api_unavailable(params, "LakeCat aoi API")
  expect_true(exists("params"))
  expect_equal(params,c("cat","ws"))
})

test_that("lc_get_params for name parameters", {
  testthat::skip_on_cran()
  params <- lc_get_params(param='metric_names')
  skip_if_api_unavailable(params, "LakeCat metric_names API")
  expect_true(exists("params"))
  expect_gt(length(params),600)
})

test_that("lc_get_params for state parameters", {
  testthat::skip_on_cran()
  params <- lc_get_params(param='state')
  skip_if_api_unavailable(params, "LakeCat state API")
  expect_true(exists("params"))
  expect_equal(nrow(params),49)
  expect_equal(names(params), c("st_fips","st_abbr","st_name"))
})

test_that("lc_get_params for county parameters", {
  testthat::skip_on_cran()
  params <- lc_get_params(param='county')
  skip_if_api_unavailable(params, "LakeCat county API")
  expect_true(exists("params"))
  expect_equal(nrow(params),3108)
  expect_equal(names(params), c("fips","state","county_name"))
})
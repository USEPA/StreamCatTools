context("Test that sc_get_nlcd is pulling in StreamCat API data")


test_that("sc_get_nlcd for a sample COMID returns a data frame", {
  testthat::skip_on_cran()
  df <- sc_get_nlcd(year='2019', aoi='cat',
                comid='179,1337,1337420')
  skip_if_api_unavailable(df, "StreamCat NLCD API")
  expect_true(exists("df"))
  expect_equal(nrow(df), 3)
  expect_equal(ncol(df), 17)
})

test_that("sc_get_nlcd for a county and ws metrics returns a data frame", {
  testthat::skip_on_cran()
  df <- sc_get_nlcd(year='2006, 2019', aoi='ws',
                county='41003')
  skip_if_api_unavailable(df, "StreamCat NLCD API")
  expect_true(exists("df"))
  expect_equal(nrow(df), 632)
  expect_equal(ncol(df), 33)
})

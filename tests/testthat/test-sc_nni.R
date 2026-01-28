context("Test that sc_nlcd is pulling in StreamCat API data")


test_that("sc_get_nni for a sample COMID returns a data frame", {
  testthat::skip_on_cran()
  df <- sc_get_nni(year='1987, 1990, 2005, 2017', aoi='cat',
                comid='1337420')
  expect_true(exists("df"))
  expect_equal(nrow(df), 1)
  expect_equal(ncol(df), 71)
})

test_that("sc_get_data for a county and ws metrics returns a data frame", {
  testthat::skip_on_cran()
  df <- sc_get_nni(year='2015, 2016, 2017', county='41003', aoi='ws')
  expect_true(exists("df"))
  expect_equal(nrow(df), 632)
  expect_equal(ncol(df), 53)
})

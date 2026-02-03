context("Test that sc_nlcd is pulling in StreamCat API data")


test_that("sc_get_data for a sample COMID returns a data frame", {
  testthat::skip_on_cran()
  df <- sc_get_nlcd(year='2019', aoi='cat',
                comid='179,1337,1337420')
  expect_true(exists("df"))
  expect_equal(nrow(df), 3)
  expect_equal(ncol(df), 17)
})

test_that("sc_get_data for a county and ws metrics returns a data frame", {
  testthat::skip_on_cran()
  df <- sc_get_nlcd(year='2006, 2019', aoi='ws',
                county='41003')
  expect_true(exists("df"))
  expect_equal(nrow(df), 632)
  expect_equal(ncol(df), 33)
})

context("Test that sc_get_data is pulling in StreamCat API data")


test_that("lc_get_data for a sample COMID returns a data frame", {
  testthat::skip_on_cran()
  df <- lc_get_data(metric='pcturbmd2006,pctconif2008,rddens', 
                    aoi='cat,ws',comid='23783629,23794487,23812618')
  expect_true(exists("df"))
  expect_equal(nrow(df), 3)
  expect_equal(ncol(df), 7)
})

test_that("lc_get_data for a county and ws metrics returns a data frame", {
  testthat::skip_on_cran()
  df <- lc_get_data(metric='pctwdwet2006', aoi='ws',
                    comid='23794487',showAreaSqKm=TRUE, showPctFull=TRUE)
  expect_true(exists("df"))
  expect_equal(nrow(df), 1)
  expect_equal(ncol(df), 5)
  })

test_that("lc_get_data for a hydroregion and ws metrics returns a data frame", {
  testthat::skip_on_cran()
  df <- lc_get_data(metric='pctwdwet2006', aoi='ws', 
                    region='Region01')
  expect_true(exists("df"))
  expect_equal(nrow(df), 12633)
  expect_equal(ncol(df), 2)
})

test_that("lc_get_data for a county and ws metrics returns a data frame", {
  testthat::skip_on_cran()
  df <- lc_get_data(metric='pctwdwet2006', aoi='ws', 
                    county='41003')
  expect_true(exists("df"))
  expect_equal(nrow(df), 23)
  expect_equal(ncol(df), 2)
})



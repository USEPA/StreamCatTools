context("Test that sc_get_data is pulling in StreamCat API data")

test_that("sc_get_data for a sample COMID returns a data frame", {
  df <- sc_get_data(comid='179', aoi='cat', metric='fert')
  expect_true(exists("df"))
  expect_equal(nrow(df), 1)
  expect_equal(ncol(df), 2)
})

test_that("sc_get_data for multiple COMIDs and areas and metrics returns a data frame", {
  df <- sc_get_data(metric='pcturbmd2006,pctconif2008,rddens', 
                    aoi='cat,ws', 
                    comid='179,1337,1337420')
  expect_true(exists("df"))
  expect_equal(nrow(df), 3)
  expect_equal(ncol(df), 7)
})

test_that("sc_get_data for showAreaSqKm and showPctFull returns a data frame", {
  df <- sc_get_data(metric='pcturbmd2006,damdens', aoi='cat,ws', 
                    comid='179,1337,1337420',showAreaSqKm=TRUE, showPctFull=TRUE)
  expect_true(exists("df"))
  expect_equal(nrow(df), 3)
  expect_equal(ncol(df), 11)
  })

test_that("sc_get_data for a county and ws metrics returns a data frame", {
  df <- sc_get_data(metric='pctwdwet2006', aoi='ws', 
                    county='41003')
  expect_true(exists("df"))
  expect_equal(nrow(df), 632)
  expect_equal(ncol(df), 2)
  })
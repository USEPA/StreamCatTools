context("Test that lc_get_watershed is returning an sf object")

test_that("lc_get_watershed is returning an sf object representing a lake watershed", {
  testthat::skip_on_cran()
  p <- lc_get_watershed(comid = 19334077, huc2 = "01",huc2_filter = "01", 
                   threads = parallel::detectCores())
  expect_true(exists("p"))
  expect_true(class(p)[1]=='sf')
})



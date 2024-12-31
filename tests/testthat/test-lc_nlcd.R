context("Test that lc_nlcd is pulling in StreamCat API data")


test_that("lc_get_data for a sample COMID returns a data frame", {
            df <- lc_nlcd(year='2001', aoi='catchment',
                              comid='23783629,23794487,23812618')
            expect_true(exists("df"))
            expect_equal(nrow(df), 3)
            expect_equal(ncol(df), 17)
})

test_that("lc_get_data for a county and ws metrics returns a data frame", {
  df <- lc_nlcd(year='2006, 2019', aoi='watershed',
                comid='23794487',showAreaSqKm=TRUE, showPctFull=TRUE)
            expect_true(exists("df"))
            expect_equal(nrow(df), 1)
            expect_equal(ncol(df), 36)
          })

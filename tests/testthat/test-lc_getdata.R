context("Test that sc_get_data is pulling in StreamCat API data")


test_that("lc_get_data for a sample COMID returns a data frame", {
            df <- lc_get_data(metric='PctUrbMd2006,pctconif2008,
                              rddens', aoi='catchment,watershed',
                              comid='23783629,23794487,23812618')
            expect_true(exists("df"))
            expect_equal(nrow(df), 3)
            expect_equal(ncol(df), 9)
})

test_that("lc_get_data for a county and ws metrics returns a data frame", {
            df <- lc_get_data(metric='pctwdwet2006', aoi='watershed',
                              comid='23794487',showAreaSqKm=FALSE, showPctFull=TRUE)
            expect_true(exists("df"))
            expect_equal(nrow(df), 1)
            expect_equal(ncol(df), 4)
          })

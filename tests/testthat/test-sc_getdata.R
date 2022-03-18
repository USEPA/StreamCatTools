context("Test that sc_get_data is pulling in StreamCat API data")


test_that("st_get_data for a sample COMID returns a data frame
          ", {
            df <- sc_get_data(metric='PctUrbMd2006,pctconif2008,
                              rddens', aoi='catchment,watershed', 
                              comid='179,1337,1337420')
            expect_true(exists("df"))
            expect_equal(nrow(df), 3)
            expect_equal(ncol(df), 9)
})


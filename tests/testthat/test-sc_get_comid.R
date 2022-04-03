context("Test that sc_get_comid is getting NHDPlusV21 comid for location data")

gages = readr::read_csv(system.file("extdata/Gages_flowdata.csv", 
                             package = "StreamCatTools"))
test_that("st_get_comid for sample gage coordinates", {
  gages_coms <- sc_get_comid(gages[1:10,], xcoord='LON_SITE',
                             ycoord='LAT_SITE', crdsys=4269)
  expect_true(exists("gages_coms"))
  expect_equal(nrow(gages_coms), 10)
  expect_equal(ncol(gages_coms), 61)
          })


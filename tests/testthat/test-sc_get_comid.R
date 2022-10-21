context("Test that sc_get_comid is getting NHDPlusV21 comid for location data")

gages = readr::read_csv(system.file("extdata/Gages_flowdata.csv", 
                             package = "StreamCatTools"))
gages <- gages[1:10,c('SOURCE_FEA','STATION_NM','LON_SITE','LAT_SITE')]
test_that("st_get_comid for sample gage coordinates", {
  gages_coms <- sc_get_comid(gages, xcoord='LON_SITE',
                             ycoord='LAT_SITE', crsys=4269)
  gages$COMID <- strsplit(gages_coms, ",")[[1]]
  expect_true(exists("gages_coms"))
  expect_equal(nrow(gages), 10)
  expect_equal(ncol(gages), 5)
          })


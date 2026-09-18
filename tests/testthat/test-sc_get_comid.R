context("Test that sc_get_comid is getting NHDPlusV21 comid for location data")

gages = readr::read_csv(system.file("extdata","Gages_flowdata.csv", 
                                                package = "StreamCatTools"))
gages <- gages[c('SOURCE_FEA','STATION_NM','LON_SITE','LAT_SITE')]

# convert to sf points (WGS84)
gages_sf <- sf::st_as_sf(gages, coords = c('LON_SITE','LAT_SITE'), crs = 4269)

test_that("sc_get_comid for sample gage coordinates (ArcGIS batch)", {
  gages_coms <- sc_get_comid(gages_sf)
  gages$COMID <- as.character(gages_coms)
  expect_true(exists("gages_coms"))
  expect_equal(nrow(gages), 9)
  expect_equal(ncol(gages), 5)
  expect_equal(length(gages$COMID), 9)
})

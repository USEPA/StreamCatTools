context("Test that lc_get_comid is getting NHDPlusV21 waterbody comid for location data")

lakes = data.frame(x = c(-89.198,-114.125,-122.044),
                   y = c(45.502,47.877,43.730)) |> 
  sf::st_as_sf(coords = c('x', 'y'), crs = 4326)

test_that("lc_get_comid for sample coordinates", {
  lake_coms <- lc_get_comid(lakes)
  expect_true(exists("lake_coms"))
  expect_true(grepl("120054065", lake_coms))
          })


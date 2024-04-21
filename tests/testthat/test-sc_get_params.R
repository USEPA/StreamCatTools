context("Test that sc_get_params is pulling in StreamCat API parameters")


test_that("sc_get_params for region parameters", {
            params <- sc_get_params(param='areaOfInterest')
            expect_true(exists("params"))
            expect_equal(params,c("catchment","other",
                                  "riparian_catchment",
                                  "riparian_watershed","watershed"))
          })

test_that("sc_get_params for name parameters", {
  params <- sc_get_params(param='name')
  expect_true(exists("params"))
  expect_equal(length(params),588)
})

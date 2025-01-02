context("Test that sc_get_params is pulling in StreamCat API parameters")


test_that("sc_get_params for region parameters", {
            params <- sc_get_params(param='areaOfInterest')
            expect_true(exists("params"))
            expect_equal(params,c("cat","catrp100","other","ws","wsrp100"))
          })

test_that("sc_get_params for name parameters", {
  params <- sc_get_params(param='name')
  expect_true(exists("params"))
  expect_equal(length(params),679)
})

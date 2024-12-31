context("Test that lc_get_params is pulling in StreamCat API parameters")


test_that("lc_get_params for region parameters", {
  params <- lc_get_params(param='areaOfInterest')
  expect_true(exists("params"))
  expect_equal(params,c("cat","ws"))
})

test_that("lc_get_params for name parameters", {
  params <- lc_get_params(param='name')
  expect_true(exists("params"))
  expect_equal(length(params),208)
})

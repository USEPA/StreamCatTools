context("Test that lc_plot is creating plot object")


test_that("lc_plot is producing basic NNI plot", {
  testthat::skip_on_cran()
  p <- lc_plotnni(comid='23794487')
  expect_true(exists("p"))
  expect_true(class(p)[1]=="patchwork")
})



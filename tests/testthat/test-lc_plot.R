context("Test that lc_plot is creating plot object")


test_that("lc_plot is producing basic NNI plot", {
  testthat::skip_on_cran()
  p <- lc_plotnni(comid='1337420')
  expect_true(exists("p"))
  expect_true(class(p)[1]=="ggplot2::ggplot")
})



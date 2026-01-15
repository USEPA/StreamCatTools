context("Test that sc_plot is creating plot object")


test_that("sc_plot is producing basic NNI plot", {
  testthat::skip_on_cran()
  p <- sc_plotnni(comid='1337420')
  expect_true(exists("p"))
  expect_true(class(p)[1]=="ggplot2::ggplot")
})

test_that("sc_plot is producing basic NNI plot", {
  testthat::skip_on_cran()
  p <- sc_plotnni(comid='1337420', include.nue=TRUE)
  expect_true(exists("p"))
  expect_true(class(p)[1]=="ggplot2::ggplot")
})

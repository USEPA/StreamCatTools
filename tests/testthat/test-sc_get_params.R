context("Test that sc_get_params is pulling in StreamCat API parameters")


test_that("sc_get_params for area of interest parameters", {
            params <- sc_get_params(param='areaOfInterest')
            expect_true(exists("params"))
            expect_equal(params,c("cat","catrp100","other","ws","wsrp100"))
          })

test_that("sc_get_params for region parameters", {
  params <- sc_get_params(param='region')
  expect_true(exists("params"))
  expect_equal(params,c("Region01","Region02","Region03N","Region03S","Region03W",
                        "Region04","Region05","Region06","Region07","Region08",
                        "Region09","Region10L","Region10U","Region11","Region12",
                        "Region13","Region14","Region15","Region16","Region17","Region18"))
})

test_that("sc_get_params for metric_names parameters", {
  params <- sc_get_params(param='metric_names')
  expect_true(exists("params"))
  expect_equal(length(params),644)
})

test_that("sc_get_params for variable_info parameters", {
  params <- sc_get_params(param='variable_info')
  expect_true(exists("params"))
  expect_equal(length(params),644)
})

test_that("sc_get_params for state parameters", {
  params <- sc_get_params(param='state')
  expect_true(exists("params"))
  expect_equal(nrow(params),49)
  expect_equal(names(params), c("st_fips","st_abbr","st_name"))
})

test_that("sc_get_params for county parameters", {
  params <- sc_get_params(param='county')
  expect_true(exists("params"))
  expect_equal(nrow(params),3108)
  expect_equal(names(params), c("fips","state","county_name"))
})


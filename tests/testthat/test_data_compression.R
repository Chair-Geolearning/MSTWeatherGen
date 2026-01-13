# Libraries:
library(testthat)
library(PTAk)

# Data :
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2012-01-01"),as.Date("2022-12-31"), by="day")

#rescompressed <- data_compression(data)
#saveRDS(rescompressed, file = "data_compressed.rds")
rescompressed <- readRDS(testthat::test_path("saved_results/data_compressed.rds"))
R_TEST_ALL <-  Sys.getenv("R_TEST_ALL") == 'TRUE'

# 0. 
test_that("The function returns a non-empty matrix", {
  expect_false(is.null(rescompressed))
  expect_true(is.matrix(rescompressed))
  expect_gt(nrow(rescompressed), 0)
  expect_gt(ncol(rescompressed), 0)
})

# 1. 
test_that("The output contains at most 4 columns and no duplicates", {
  expect_lte(ncol(rescompressed), 4)
  expect_equal(ncol(rescompressed), ncol(unique(rescompressed, MARGIN = 2)))
})

# 2.  
test_that("All output values are finite", {
  expect_false(any(!is.finite(rescompressed)))
})

# 3.
test_that("The function handles a sub-sample", {
  if (!R_TEST_ALL) {
    skip('skip')
  }
  skip_on_cran()  
  skip_on_ci()

  sub_data <- data[,1,, drop = FALSE]  
  res_sub <- data_compression(sub_data)
  
  expect_true(is.matrix(res_sub))
  expect_lte(ncol(res_sub), 4)
})

# 4.
test_that("The function handles a single variable", {
  
  if (!R_TEST_ALL) {
    skip('skip')
  }
  skip_on_cran()
  skip_on_ci()
  
  sub_data <- data[,,1, drop = FALSE]  
  res_sub <- data_compression(sub_data)
  expect_true(is.matrix(res_sub))
  expect_lte(ncol(res_sub), 4)
})

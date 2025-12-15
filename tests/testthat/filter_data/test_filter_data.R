# Libraries:
library(testthat)
library(MSTWeatherGen)  

# Data :
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"),as.Date("2021-12-31"), by="day")
summer <- list(min_day = 1, max_day = 31, min_month = 6, max_month = 8)

# 0.
test_that("The argument types are correct", {
  
  expect_is(data, "array")
  expect_is(dates, "Date")
  expect_is(season, "list")
})

# 1.
test_that("filter_season_data returns the correct dimensions", {
  result <- filter_season_data(data, dates, summer, names = names)
  
  # Check dimensions
  expect_equal(dim(result$data_filtered)[2],dim(data)[2] )  # Spatial variables unchanged
  expect_equal(dim(result$data_filtered)[3],dim(data)[3] ) # Meteorological variables unchanged
  expect_equal(length(result$dates_filtered),dim(result$data_filtered)[1] ) # Date dimensions preserved


})

# 2.
test_that("Structure of the filtered data", {
  result <- filter_season_data(data, dates, season)
  
  expect_is(result$data_filtered, "array")
  expect_equal(length(result$dates_filtered), dim(result$data_filtered)[1])
  
  
})

# 3.
test_that("Empty data", {
  empty_data <- array(NA, dim = c(0, 3, 3))  # Empty table
  season <- list(min_day = 1, max_day = 31, min_month = 1, max_month = 12)
  expect_error(result <- filter_season_data(empty_data, dates, season))
  
  #expect_equal(length(result$data_filtered), 0)
  #expect_equal(length(result$dates_filtered), 0)
})


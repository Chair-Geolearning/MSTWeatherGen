# Libraries:
library(testthat)
library(MSTWeatherGen)  

# Data to be tested on:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"),as.Date("2021-12-31"), by="day")
summer <- list(min_day = 1, max_day = 31, min_month = 6, max_month = 8)

test_that("les types des arguments sont corrects", {
  
  expect_is(data, "array")
  expect_is(dates, "Date")
  expect_is(season, "list")
})

# 1
test_that("filter_season_data renvoie les bonnes dimensions", {
  result <- filter_season_data(data, dates, summer, names = names)
  
  # Vérifier dimensions
  expect_equal(dim(result$data_filtered)[2],dim(data)[2] )  # variable spatiales non changees.
  expect_equal(dim(result$data_filtered)[3],dim(data)[3] ) # variable meteorologiques non changees.
  expect_equal(length(result$dates_filtered),dim(result$data_filtered)[1] ) # dimensions de dates preservees.


})

# 2
test_that("Structure des données filtrées", {
  result <- filter_season_data(data, dates, season)
  
  expect_is(result$data_filtered, "array")
  expect_equal(length(result$dates_filtered), dim(result$data_filtered)[1])
  
  
})

# 3
test_that("Données vides", {
  empty_data <- array(NA, dim = c(0, 3, 3))  # tableau vide
  empty_dates <- as.Date(character(0))      # vecteur de dates vide
  season <- list(min_day = 1, max_day = 31, min_month = 1, max_month = 12)
  result <- filter_season_data(empty_data, empty_dates, season)
  
  expect_equal(length(result$data_filtered), 0)
  expect_equal(length(result$dates_filtered), 0)
})


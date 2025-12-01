# Libraries:
library(testthat)
library(MSTWeatherGen)  

# Data to be tested on:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2012-01-01"),as.Date("2022-12-31"), by="day")

resultperm <- readRDS("resultperm2.rds")

# Saved results if you want to reproduce them.

# resultperm <- weather_types(
# data = data,
#  variables = c("Wind", "Temp_max"),
# dates = dates,
# n_wt = 5,
# coordinates = coordinates,
# return_plots = FALSE,
# names_units = c("m/s", "°C"),
#  dir = tempdir()
# )



# 1. 
test_that("weather_types returns a cluster vector and works even if return_plots = FALSE", {
  n_wt <- 3
  res <- weather_types(
    data = data,
    variables = c("Wind", "Temp_max"),
    dates = dates,
    n_wt = n_wt,
    coordinates = coordinates,
    return_plots = FALSE,
    names_units = c("m/s", "°C"),
    dir = tempdir()
  )
  
  expect_type(res$cluster, "double")                    # type of the result
  expect_equal(length(res$cluster), dim(data)[1])       # one label per day
  expect_equal(length(unique(res$cluster)), n_wt)       # exactly n_wt clusters
  expect_true(all(res$cluster %in% 1:n_wt))             # integers comprised between 1 and n_wt
})

# 2. Structure des résultats"
test_that("Structure des résultats", {
  result <- weather_types(
    data = data,
    variables = c("Wind", "Temp_max"),
    dates = dates,
    n_wt = 3,
    coordinates = coordinates,
    return_plots = FALSE,
    names_units = c("m/s", "°C"),
    dir = tempdir()
  )
  
  expect_is(result, "list")
  expect_equal(length(result),2)
  expect_true(is.numeric(result))
  
})

# 3. Test that similar results when similar experience
test_that("test that similar results when similiar experience, because we are fixing the seed now", {
  set.seed(123)
  result_first<- weather_types(data = data,
                          variables = c("Wind", "Temp_max"),
                          dates = dates,
                          n_wt = 5,
                          coordinates = coordinates,
                          return_plots = FALSE,
                          names_units = c("m/s", "°C"),
                          dir = tempdir())
  # Second try.
  set.seed(123)
  result_second <- weather_types(data = data,
                          variables = c("Wind", "Temp_max"),
                          dates = dates,
                          n_wt = 5,
                          coordinates = coordinates,
                          return_plots = FALSE,
                          names_units = c("m/s", "°C"),
                          dir = tempdir())
  
  expect_equal(result_first$cluster,result_second$cluster)
  
})


# 4. What happened when n_wt is equal to zero, it should be working 
test_that("What happened when n wt and max nw  is equal to zero", {
  expect_error( result <- weather_types(
    data = data,
    variables = c("Wind", "Temp_max"),
    dates = dates,
    n_wt = 0,
    coordinates = coordinates,
    return_plots = FALSE,
    names_units = c("m/s", "°C"),
    dir = tempdir()
  ))
})

# 5. What happened when max nwt is equal to zero, it should be working 
test_that("What happened when n wt and max nw  is equal to zero", {
  expect_error( result <- weather_types(
    data = data,
    variables = c("Wind", "Temp_max"),
    dates = dates,
    max_number_wt = 0,
    coordinates = coordinates,
    return_plots = FALSE,
    names_units = c("m/s", "°C"),
    dir = tempdir()
  ))
})



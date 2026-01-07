# Libraries:
library(testthat)
library(mclust)

# Data :
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2012-01-01"),as.Date("2022-12-31"), by="day")

# Parameters:
testall <- FALSE

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

resultperm <- readRDS("saved_results/resultperm2.rds")
n_wt <- 3

'result <- weather_types(
  data = data,
  variables = c("Wind", "Temp_max"),
  dates = dates,
  n_wt = n_wt,
  coordinates = coordinates,
  return_plots = FALSE,
  names_units = c("m/s", "°C"),
  dir = tempdir()
)'

result <- readRDS("saved_results/result_wt.rds")

# 0. 
test_that("weather_types returns a cluster vector and works even if return_plots = FALSE", {
  expect_type(result$cluster, "double")                    
  expect_equal(length(result$cluster), dim(data)[1])      
  expect_equal(length(unique(result$cluster)), n_wt)       
  expect_true(all(result$cluster %in% 1:n_wt))             
})

# 1.
test_that("Results structure", {
 
  expect_type(result, "list")
  expect_equal(length(result),2)
  expect_true(is.numeric(result$cluster))
  
})

# 2. 
test_that("test that similar results when having similiar experience, because we are fixing the seed now", {
  skip_on_cran()
  if (!testall) {
    skip('skip')
  }

  set.seed(123)
  result_first<- weather_types(data = data,
                          variables = c("Wind", "Temp_max"),
                          dates = dates,
                          n_wt = 5,
                          coordinates = coordinates,
                          return_plots = FALSE,
                          names_units = c("m/s", "°C"),
                          dir = tempdir())

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


# 3. 
test_that("What happened when n wt is equal to zero", {
  skip_on_cran()
  
  if (!testall) {
    skip('skip')
  }
  
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

# 4. 
test_that("What happened when max nwt is equal to zero", {
  skip_on_cran()
  
  if (!testall) {
    skip('skip')
  }
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



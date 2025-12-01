# Libraries:
library(testthat)

# Data to be tested on:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"),as.Date("2021-12-31"), by="day")
names = c("Precipitation", "Wind", "Temp_max")
resultperm <- readRDS("resultperm2.rds")
set.seed(1)
wt <- resultperm$cluster
K <- length(unique(wt))
            

# 1. Test structure globale
test_that("estimate_lambda_transformations returns a list with expected components", {
  res <- estimate_lambda_transformations(
    data = data,
    wt = wt,
    names = names,
    coordinates = coordinates
  )
  
  expect_type(res, "list")
  expect_equal(length(res), 2)
  expect_true(all(c("lambda_transformations", "threshold_precip") %in% names(res)))
  expect_equal(length(res$lambda_transformations), length(unique(wt)))
})


# 2. Test structure interne par WT / variable / location
test_that("lambda_transformations has correct internal structure", {
  res <- estimate_lambda_transformations(
    data = data,
    wt = wt,
    names = names,
    coordinates = coordinates)
  ns <- dim(data)[2]
  nv <- length(names)
  
  for(k in 1:K){
    expect_equal(length(res$lambda_transformations[[k]]), nv)
    for(v in 1:nv){
      expect_equal(length(res$lambda_transformations[[k]][[v]]), ns)
      for(j in 1:ns){
        expect_true("q" %in% names(res$lambda_transformations[[k]][[v]][[j]]))
      }
    }
  }
})


# 3. Test structure interne par WT / variable / location
test_that("threshold_precip est numérique et positif", {
  res <- estimate_lambda_transformations(
    data = data,
    wt = wt,
    names = names,
    coordinates = coordinates
  )
  ns <- dim(data)[2]
  for(k in 1:K){
    
    expect_equal(length(res$threshold_precip[[k]]), ns)
  }
  
  expect_type(res$threshold_precip, "list")
  expect_true(all(is.finite(unlist(res$threshold_precip)))) # 3.Check of the finitude if the threshold
})


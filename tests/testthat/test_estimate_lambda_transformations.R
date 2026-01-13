# Libraries:
library(testthat)

# Data :
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"),as.Date("2021-12-31"), by="day")
names = c("Precipitation", "Wind", "Temp_max")

# Retrieving results:
resultperm <- readRDS(testthat::test_path("saved_results/resultperm2.rds"))
set.seed(1)
wt <- resultperm$cluster
K <- length(unique(wt))

# ----- Testing -----            
res <- estimate_lambda_transformations(
  data = data,
  wt = wt,
  names = names,
  coordinates = coordinates
)

# 0. 
test_that("estimate_lambda_transformations returns a list with expected components", {
  
  expect_type(res, "list")
  expect_equal(length(res), 2)
  expect_true(all(c("lambda_transformations", "threshold_precip") %in% names(res)))
  expect_equal(length(res$lambda_transformations), length(unique(wt)))
})

# 1.  
test_that("lambda_transformations has correct internal structure", {
  ns <- dim(data)[2]
  nv <- length(names)
  
  lapply(seq_len(K), function(k) {
    expect_equal(length(res$lambda_transformations[[k]]), nv)
    
    lapply(seq_len(nv), function(v) {
      expect_equal(length(res$lambda_transformations[[k]][[v]]), ns)
      
      lapply(seq_len(ns), function(j) {
        expect_true(
          "q" %in% names(res$lambda_transformations[[k]][[v]][[j]])
        )
      })
    })
  })
})

# 2. 
test_that("threshold_precip is numeric and positive", {
  ns <- dim(data)[2]
  
  lapply(1:K, function(k) {
    expect_equal(length(res$threshold_precip[[k]]), ns)
  })

  expect_type(res$threshold_precip, "list")
  expect_true(all(is.finite(unlist(res$threshold_precip)))) # 3.Check of the finitude if the threshold
})


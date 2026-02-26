# Libraries:
library(testthat)

# Data:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"),as.Date("2021-12-31"), by="day")
names = c("Precipitation", "Wind", "Temp_max")
seasons <- list(
  s1 = list(min_day = 1, max_day = 29, min_month = 12, max_month = 2),
  s2 = list(min_day = 1, max_day = 31, min_month = 3, max_month = 5),
  s3 = list(min_day = 1, max_day = 31, min_month = 6, max_month = 8),
  s4 = list(min_day = 1, max_day = 30, min_month = 9, max_month = 11)
)

# Parameters:
uh <- readRDS(testthat::test_path("saved_results/uh.rds"))
par = c(1, 1)
h <- readRDS(testthat::test_path("saved_results/h.rds")) 

# -------------------------------------------------Tests -----------------------------------------------------------
result <- loglik_spatial(par = c(1, 1), data = data, 
                         h = h, uh = uh, v = 2)
# 0.
test_that("loglik_spatial retourne un scalaire numerique", {
  
  expect_length(result, 1)
  expect_true(is.numeric(result))
})

# 1.
test_that("loglik_spatial retourne une penalite si par[1] < 0", {
  result <- loglik_spatial(par = c(-1, 1), data = data, 
                            h = h, uh = uh, v = 2)
  expect_gt(result, 1e+15)
})

# 2.
test_that("loglik_spatial retourne une penalite si par[2] < 0", {
  result <- loglik_spatial(par = c(1, -1), data = data, 
                           h = h, uh = uh, v = 2)
  expect_gt(result, 1e+15)
})

# 3.
test_that("loglik_spatial retourne une valeur positive (negative log-lik)", {
  result <- loglik_spatial(par = c(-1, 1), data = data, 
                           h = h, uh = uh, v = 2)
  expect_gt(result, 0)
})

# 4.
test_that("loglik_spatial ne retourne pas Inf ni NaN", {
  result <- loglik_spatial(par = c(-1, 1), data = data, 
                           h = h, uh = uh, v = 2)
  
  expect_false(is.infinite(result))
  expect_false(is.nan(result))
})

# 5.
'test_that("loglik_spatial diminue avec de meilleurs parametres", {
  result1 <- loglik_spatial(par = c(0.01, 0.1), data = data, 
                          h = h, uh = uh, v = 2)
  
  result2 <- loglik_spatial(par = c(1, 1), data = data, 
                            h = h, uh = uh, v = 2)
  
  expect_gt(result2,result1)
})'

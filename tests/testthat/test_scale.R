# Libraries:
library(testthat)

# Data:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2012-01-01"),as.Date("2022-12-31"), by="day")

# --- Test of the moving average sub-function ---
# Define a moving average function

moving_average <- function(x, n) {
  len <- length(x)
  avg <- rep(NA, len)
  
  for (i in 1:len) {
    lower <- max(1, i - n %/% 2)
    upper <- min(len, i + n %/% 2)
    window <- x[lower:upper]
    avg[i] <- mean(window, na.rm = TRUE)
  }
  
  return(avg)
}

# 0.
test_that("moving_average n=1 is identity on real Temp_max data, same with n=0", {
  x <- data[, 1, "Temp_max"]
  
  expect_equal(unname(moving_average(x, 0)), unname(x)) # moving_average n=1 is identity on real Temp_max data, same with n=0
  expect_equal(unname(moving_average(x, 1)), unname(x)) # We unnamed everything as it can create an error with names.
  expect_equal(unname(moving_average(x, 2)), unname(moving_average(x, 3))) # Test on even number, which should be equal to 3 in this case.

})

# 1.
test_that("moving_average handles edges correctly on the edges.", {
  x <- data[, 1, "Wind"]
  res <- moving_average(x, 5)
  
  expect_equal(res[1], mean(x[1:3], na.rm = TRUE))
  expect_equal(res[2], mean(x[1:4], na.rm = TRUE))
  expect_equal(res[length(x)], mean(x[(length(x)-2):length(x)], na.rm = TRUE))
  
})

# 2.
test_that("moving_average output has same length and type", {
  x <- data[, 1, "Temp_max"]
  res <- moving_average(x, 7)
  expect_equal(length(res), length(x))
  expect_true(is.numeric(res))
})

# 3.
test_that("moving_average handles NA values correctly", {
  x <- data[, 1, "Temp_max"]
  x[100] <- NA
  x[102] <- NA
  res <- moving_average(x, 3)
  
  expect_equal(res[100], mean(x[c(99,100,101)], na.rm = TRUE))
  expect_equal(res[102], mean(x[c(101,102,103)], na.rm = TRUE))
})


# --- Test for Scaled Data Function ---

# 0.
test_that("Results structure", {
  result <- scale_data(data, names, dates)
  
  expect_type(result, "list")
  expect_equal(length(result), 2)
  expect_named(result, c("data", "scale_parm"))
  expect_equal(dim(result$data), dim(data))
  expect_false(all(result$data  ==  data)) # We check that the data has been modified
  expect_equal(length(result$scale_parm), 2)
  expect_equal(length(result$scale_parm$mu), length(result$scale_parm$sd))
  expect_equal(length(result$scale_parm$mu), dim(data)[3]-1) # We do no take into account precipitation.
  
})

# 1.
test_that("Period too short, expect warning", {
  datesbis = seq(as.Date("2019-01-01"),as.Date("2022-12-31"), by="day")

  expect_warning(
    result <- scale_data(data, names, datesbis)
  )
  
})

# 2. 
test_that("window_size = 1 or 0 does not smooth seasonal means", {
  res1  <-scale_data(data, names, dates, window_size = 1)
  res0  <-scale_data(data, names, dates, window_size = 0)
  res30 <-scale_data(data, names, dates, window_size = 30)
  # Testing a variable (not Precipitation).
  mu0  <- res0$scale_parm$mu$Wind
  mu1  <- res1$scale_parm$mu$Wind
  mu30 <- res30$scale_parm$mu$Wind
  
  # Verify that a different window produces a different result.
  expect_false(all(mu1 == mu30))
  expect_false(all(mu0 == mu30))
  expect_true(all(mu0 == mu1))
  # mu0 should be equal to mu1.
  expect_equal(mu1, mu0, tolerance = 1e-12)
  expect_false(all(res0$data == data))
  expect_failure(expect_equal(res1$data, res30$data, tolerance = 1e-12))
  
})




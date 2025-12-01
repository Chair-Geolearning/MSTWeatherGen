# Libraries:
library(testthat)
library(MSTWeatherGen)  

# Data to be tested on:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2012-01-01"),as.Date("2022-12-31"), by="day")

# Test de la sous-fonction moving average :
test_that("moving_average n=1 is identity on real Temp_max data, same with n=0", {
  x <- data[, 1, "Temp_max"]
  
  expect_equal(unname(moving_average(x, 0)), unname(x)) #moving_average n=1 is identity on real Temp_max data, same with n=0
  expect_equal(unname(moving_average(x, 1)), unname(x)) #We unname 
  expect_equal(unname(moving_average(x, 2)), unname(moving_average(x, 3))) # Test on even number, which should be equal to 3 in this case.

})

# Edge case :
test_that("moving_average handles edges correctly on the edges.", {
  x <- data[, 1, "Wind"]
  res <- moving_average(x, 5)
  
  expect_equal(res[1], mean(x[1:3], na.rm = TRUE))
  expect_equal(res[2], mean(x[1:4], na.rm = TRUE))
  expect_equal(res[length(x)], mean(x[(length(x)-2):length(x)], na.rm = TRUE))
  
})

#Type and length
test_that("moving_average output has same length and type", {
  x <- data[, 1, "Temp_max"]
  res <- moving_average(x, 7)
  expect_equal(length(res), length(x))
  expect_true(is.numeric(res))
})

# Handling of NA values.
test_that("moving_average handles NA values correctly", {
  x <- data[, 1, "Temp_max"]
  x[100] <- NA
  x[102] <- NA
  res <- moving_average(x, 3)
  
  expect_equal(res[100], mean(x[c(99,100,101)], na.rm = TRUE))
  expect_equal(res[102], mean(x[c(101,102,103)], na.rm = TRUE))
})


# ---Scaled Data Function ---

# 0.
test_that("Structure des résultats", {
  result <- scale_data(data, names, dates)
  
  expect_is(result, "list")
  expect_equal(length(result), 2)
  expect_named(result, c("data", "scale_parm"))
  expect_equal(dim(result$data), dim(data))
  expect_false(all(result$data  ==  data)) # La donnee a bien ete modifiee. 
  expect_equal(length(result$scale_parm), 2)
  expect_equal(length(result$scale_parm$mu), length(result$scale_parm$sd))
  expect_equal(length(result$scale_parm$mu), dim(data)[3]-1) # On prend pas en compte la precipitation.
  
})

# 1.
test_that("Periode trop courte, expect warning", {
  datesbis = seq(as.Date("2019-01-01"),as.Date("2022-12-31"), by="day")
  
  expect_warning(
    result <- scale_data(data, names, datesbis)
  )
  
})

# 2.  
test_that("Structure des résultats", {
  result <- scale_data(data, names, dates)
  
  expect_is(result, "list")
  expect_equal(length(result), 2)
  expect_named(result, c("data", "scale_parm"))
  expect_equal(dim(result$data), dim(data))
  expect_false(all(result$data  ==  data)) # La donnee a bien ete modifiee. 
  expect_equal(length(result$scale_parm), 2)
  expect_equal(length(result$scale_parm$mu), length(result$scale_parm$sd))
  expect_equal(length(result$scale_parm$mu), dim(data)[3]-1) # On prend pas en compte la precipitation.

})

# 3.
test_that("Periode trop courte, expect warning", {
  datesbis = seq(as.Date("2019-01-01"),as.Date("2022-12-31"), by="day")

  expect_warning(
    result <- scale_data(data, names, datesbis)
  )
  
})

# 4. 
test_that("window_size = 1 or 0 does not smooth seasonal means", {
  res1  <-scale_data(data, names, dates, window_size = 1)
  res0  <-scale_data(data, names, dates, window_size = 0)
  res30 <-scale_data(data, names, dates, window_size = 30)
  # On teste une variable lissée (donc pas Precipitation)
  mu0  <- res0$scale_parm$mu$Wind
  mu1  <- res1$scale_parm$mu$Wind
  mu30 <- res30$scale_parm$mu$Wind
  
  # Vérifie qu’une fenêtre différente produit un résultat différent.
  expect_false(all(mu1 == mu30))
  expect_false(all(mu0 == mu30))
  expect_true(all(mu0 == mu1))
  # mu0 doit être identique à mu1.
  expect_equal(mu1, mu0, tolerance = 1e-12)
  expect_false(all(res0$data == data))
  expect_failure(expect_equal(res1$data, res30$data, tolerance = 1e-12))
  
})




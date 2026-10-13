library(testthat)

# Data
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names <- c("Precipitation", "Wind", "Temp_max")

test_that("set_spatial_bounds modifie bien .lower et .upper dans le namespace", {
  
  coords_test <- data.frame(longitude = c(0, 1, 10), latitude = c(0, 0, 0))
  set_spatial_bounds(coords_test)
  
  # Pour eviter que 1/0 = inf
  dst_min <- min(dst[dst > 0])
  dst_max <- max(dst)
  
  lower_aii <- 1 / dst_max
  upper_aii <- 1 / dst_min
  
  ns <- getNamespace("MSTWeatherGen")
  
  expect_equal(get(".lower", envir = ns)[.aii],              lower_aii, tolerance = 1e-6)
  expect_equal(get(".lower", envir = ns)[.ajj],              lower_aii, tolerance = 1e-6)
  expect_equal(get(".upper", envir = ns)[.aii],              upper_aii, tolerance = 1e-6)
  expect_equal(get(".upper", envir = ns)[.ajj],              upper_aii, tolerance = 1e-6)
  expect_equal(get(".lower_spatial", envir = ns)[.matern_a], lower_aii, tolerance = 1e-6)
  expect_equal(get(".upper_spatial", envir = ns)[.matern_a], upper_aii, tolerance = 1e-6)
})

test_that("set_spatial_bounds : les autres bornes ne sont pas modifiées", {
  
  ns <- getNamespace("MSTWeatherGen")
  
  lower_avant <- get(".lower", envir = ns)
  upper_avant <- get(".upper", envir = ns)
  
  coords_test <- data.frame(longitude = c(0, 1, 10), latitude = c(0, 0, 0))
  set_spatial_bounds(coords_test)
  
  indices_autres <- setdiff(seq_along(.lower), c(.aii, .ajj))
  
  expect_equal(get(".lower", envir = ns)[indices_autres], lower_avant[indices_autres])
  expect_equal(get(".upper", envir = ns)[indices_autres], upper_avant[indices_autres])
})



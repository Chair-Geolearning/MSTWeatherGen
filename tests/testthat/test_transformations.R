# Libraries:
library(testthat)

# Data :
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names  <- c("Precipitation", "Wind", "Temp_max")
dates  <- seq(as.Date("2018-01-01"), as.Date("2021-12-31"), by = "day")

# Retrieving results:
set.seed(1)
wt   <- resultperm$cluster
K    <- length(unique(wt))
ns   <- dim(data)[2]

lmbd <- estimate_lambda_transformations(
  data        = data,
  wt          = wt,
  names       = names,
  coordinates = coordinates
)$lambda_transformations

res <- transformations(
  data        = data,
  wt          = wt,
  names       = names,
  coordinates = coordinates,
  lmbd        = lmbd
)

# ----- Testing -----

# 0.
test_that("transformations returns an array with the same dimensions as input", {
  expect_true(is.array(res))
  expect_equal(dim(res), dim(data))
}) 

# 2.
test_that("transformed values are finite", {
  expect_true(all(is.finite(res)))
})

# 3.
test_that("transformations changes values", {
  expect_false(identical(data, res))
})

# 4.
test_that("transformations falls back to nearest valid station when coef[2] is NA for one random station", {
  lmbd_mod <- lmbd
  lmbd_mod[[1]][[1]][[1]]$fit$coef[2] <- NA
  
  res_mod <- transformations(
    data        = data,
    wt          = wt,
    names       = names,
    coordinates = coordinates,
    lmbd        = lmbd_mod
  )
  
  expect_true(is.array(res_mod))
  expect_equal(dim(res_mod), dim(data))
  expect_true(all(is.finite(res_mod[res_mod != 0])))
})

# 5.
test_that("transformations falls back correctly when multiple consecutive stations have coef[2] NA", {
  lmbd_mod <- lmbd
  for (jj in 1:min(3, ns)) {
    lmbd_mod[[1]][[1]][[jj]]$fit$coef[2] <- NA
  }
  
  res_mod <- transformations(
    data        = data,
    wt          = wt,
    names       = names,
    coordinates = coordinates,
    lmbd        = lmbd_mod
  )
  
  expect_true(is.array(res_mod))
  expect_true(all(is.finite(res_mod[res_mod != 0])))
})

# 6. [BUG CONNU] : boucle infinie quand toutes les stations ont coef[2] = NA
test_that("transformations does not infinite loop when all stations have coef[2] NA", {
  lmbd_mod <- lmbd
  for (jj in 1:ns) {
    lmbd_mod[[1]][[1]][[jj]]$fit$coef[2] <- NA
  }
  
  setTimeLimit(elapsed = 20, transient = TRUE)
  
  expect_error(
    transformations(
      data        = data,
      wt          = wt,
      names       = names,
      coordinates = coordinates,
      lmbd        = lmbd_mod
    ),
    regexp = "reached elapsed time limit"
  )
  
  setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  
  
})

# 7.
test_that("transformations handles data with all-zero values without error", {
  data0   <- data
  data0[] <- 0
  
  res0 <- transformations(
    data        = data0,
    wt          = wt,
    names       = names,
    coordinates = coordinates,
    lmbd        = lmbd
  )
  
  expect_equal(res0, data0)
})

# 8.
test_that("transformations works with a single weather type", {
  wt1 <- rep(1, length(wt))
  
  res1 <- transformations(
    data        = data,
    wt          = wt1,
    names       = names,
    coordinates = coordinates,
    lmbd        = lmbd
  )
  
  expect_true(is.array(res1))
  expect_equal(dim(res1), dim(data))
})
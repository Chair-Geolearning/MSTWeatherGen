# Libraries:
library(testthat)
library(MSTWeatherGen)  

# Data:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"),as.Date("2021-12-31"), by="day")
names = c("Precipitation", "Wind", "Temp_max")

# Parameters: 
set.seed(1)
wt <- resultperm$cluster
K <- length(unique(wt))
threshold_precip <- c(0, 1, 2)
n1 <- 2
n2 <- 2
max_it <- 5
tmax <- 2
ax <- list(
  v1  = c("Wind", "Wind", "Temp"),
  v2  = c("Wind", "Temp", "Temp"),
  cov = c(0.01, 0.01, 0.01)
)
cr <- diag(length(names))
vgm <-spacetime_cov(
  data = data[,,variable],
  wt_id = 50:200,
  locations = coordinates,
  ds = dst,
  dates = dates,
  lagstime = 0,
  dist = dist,
  covgm = TRUE
)

ax <- vgm[vgm$lagtime==0&vgm$dist==max(vgm$dist),]

# --- Tests for function Estimation gf ----

# Dimensions
Nt <- dim(data)[1]
Ns <- dim(data)[2]
Nv <- dim(data)[3]

# 0.
test_that("estimation_gf returns a list with parm and par_all", {
  res <- estimation_gf(
    data = data,
    wt_id = 110:150,
    max_it = max_it,
    dates = dates,
    tmax = tmax,
    names = names,
    coordinates = coordinates,
    n1 = n1,
    n2 = n2,
    ax = ax,
    cr = cr,
    threshold_precip = threshold_precip
  )
  
  expect_type(res, "list")
  expect_named(res, c("parm", "par_all"))
})

# 1.
test_that("parm is a matrix with correct dimensions", {
  res <- estimation_gf(
    data = data,
    wt_id = 1:length(dates),
    max_it = max_it,
    dates = dates,
    tmax = tmax,
    names = names,
    coordinates = coordinates,
    n1 = n1,
    n2 = n2,
    ax = ax,
    cr = cr,
    threshold_precip = threshold_precip
  )
  
  expect_true(is.matrix(res$parm))
  expect_equal(nrow(res$parm), length(names)^2)
})

# 2.
test_that("par_all length is greater than zero", {
  res <- estimation_gf(
    data = data,
    wt_id = 1:length(dates),
    max_it = max_it,
    dates = dates,
    tmax = tmax,
    names = names,
    coordinates = coordinates,
    n1 = n1,
    n2 = n2,
    ax = ax,
    cr = cr,
    threshold_precip = threshold_precip
  )
  
  expect_true(length(res$par_all) > 0)
})

# 3.
test_that("estimation_gf handles missing par_all argument", {
  expect_silent(
    res <- estimation_gf(
      data = data,
      wt_id = 1:length(dates),
      max_it = max_it,
      dates = dates,
      tmax = tmax,
      names = names,
      coordinates = coordinates,
      n1 = n1,
      n2 = n2,
      ax = ax,
      cr = cr,
      threshold_precip = threshold_precip
    )
  )
})

# 4.
test_that("parm values are numeric", {
  res <- estimation_gf(
    data = data,
    wt_id = 1:length(dates),
    max_it = max_it,
    dates = dates,
    tmax = tmax,
    names = names,
    coordinates = coordinates,
    n1 = n1,
    n2 = n2,
    ax = ax,
    cr = cr,
    threshold_precip = threshold_precip
  )
  
  expect_true(all(is.numeric(res$parm)))
})

# Librairies:
library(testthat)
library(MSTWeatherGen)  

# Donnees:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"),as.Date("2021-12-31"), by="day")
names = c("Precipitation", "Wind", "Temp_max")

resultperm <- readRDS("resultperm2.rds")

set.seed(1)
wt <- resultperm$cluster
K <- length(unique(wt))

# --- Tests sous-fonction Estimation gf ----

# Récupération des dimensions réelles
Nt <- dim(data)[1]
Ns <- dim(data)[2]
Nv <- dim(data)[3]

# Paramètres fictifs pour les tests
threshold_precip <- c(0, 1, 2)
n1 <- 2
n2 <- 2
max_it <- 5
tmax <- 2
ax <- matrix(1, length(names), length(names))
cr <- diag(length(names))

# 0.
test_that("estimation_gf returns a list with parm and par_all", {
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

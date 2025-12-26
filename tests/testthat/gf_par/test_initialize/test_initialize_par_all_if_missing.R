# Libraries:
library(testthat)

# Data:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"),as.Date("2021-12-31"), by="day")
names = c("Precipitation", "Wind", "Temp_max")
names_bis = c("Wind", "Temp_max")

# Retrieve results
resultperm <- readRDS("/home/aboualam/MSTWeatherGen/tests/testthat/resultperm2.rds")
wt <- resultperm$cluster
K <- length(unique(wt))

# Dimensions
Nt <- dim(data)[1]
Ns <- dim(data)[2]
Nv <- dim(data)[3]

# Parameters
names <- c("Wind", "Temp")
pairs <- c("Wind-Wind", "Wind-Temp", "Temp-Temp")

par_s <- matrix(
  c(1.0, 0.3,
    0.3, 1.0),
  nrow = 2,
  byrow = TRUE
)

# ax : valeurs petites NON NULLES
ax <- list(
  v1  = c("Wind", "Wind", "Temp"),
  v2  = c("Wind", "Temp", "Temp"),
  cov = c(0.01, 0.01, 0.01)
)

cr <- diag(length(names))

'vgm <-spacetime_cov(
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

cr <- sapply(names, function(v1) {
  sapply(names, function(v2) {
    mean(sapply(1:dim(data)[2], function(j) cor(data[, j, v1], data[, j, v2], use = "complete.obs")), na.rm = TRUE)
  })
})'

# 0.
test_that("initialize_par_all_if_missing runs without error", {
  
  expect_silent({
    par_all <- initialize_par_all_if_missing(
      par_all = NULL,
      names = names,
      pairs = pairs,
      par_s = par_s,
      ax = ax,
      cr = cr
    )
  })
  
  expect_type(par_all, "double")
  expect_false(anyNA(par_all))
  expect_true(length(par_all) > 0)
  expect_true(all(is.finite(par_all)))
  
})

# 1.
test_that("initialize_par_all_if_missing creates expected parameter names", {
  
  par_all <- initialize_par_all_if_missing(
    par_all = NULL,
    names = names,
    pairs = pairs,
    par_s = par_s,
    ax = ax,
    cr = cr
  )
  
  expected_names <- c(
    paste(pairs, "dij", sep=":"),
    paste(pairs, "rij", sep=":"),
    paste(pairs, "vij", sep=":"),
    paste(pairs, "ax",  sep=":")
  )
  
  expect_true(all(expected_names %in% names(par_all)))
})

# 2.
test_that("existing par_all is not overwritten", {
  
  par_all_init <- setNames(rep(0.5, 5), paste0("p", 1:5))
  
  par_all <- initialize_par_all_if_missing(
    par_all = NULL,
    names = names,
    pairs = pairs,
    par_s = par_s,
    ax = ax,
    cr = cr
  )
  
  par_all_bis <- initialize_par_all_if_missing(
    par_all = par_all,
    names = names,
    pairs = pairs,
    par_s = par_s,
    ax = ax,
    cr = cr
  )
  
  expect_equal(par_all_bis[names(par_all)], par_all)
})

# 3.
'test_that("resulting beta matrix is positive definite", {
  
  par_all <- initialize_par_all_if_missing(
    par_all = NULL,
    names = names,
    pairs = pairs,
    par_s = par_s,
    ax = ax,
    cr = cr
  )
  
  parm <- param(par_all, names_vars)
  beta <- compute_beta(parm, names_vars, cr_ok)
  
  expect_true(all(eigen(beta, symmetric = TRUE)$values > 0))
})'


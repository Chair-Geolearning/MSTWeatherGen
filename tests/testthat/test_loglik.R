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
s1 = list(min_day = 1, max_day = 29, min_month = 12, max_month = 2)

par_all <- readRDS(testthat::test_path("saved_results/par_all.rds"))
Vi <- readRDS(testthat::test_path("saved_results/Vi.rds"))
uh <- readRDS(testthat::test_path("saved_results/uh.rds"))
uh <- uh[uh[,1]==0,]

cr <- readRDS(testthat::test_path("saved_results/cr.rds"))
ep <- readRDS(testthat::test_path("saved_results/ep.rds"))
max_it <- 50

parms <- c("a1", "d1", "g1", "a2", "d2", "g2",
           "b1", "e1", "l1", "b2", "e2", "l2", "c", "f", "m",
           paste(names, "ai", sep = ":"), paste(names, "bi", sep = ":"),
           paste(names, "ci", sep = ":"))
par <- par_all[parms] 
u <- NULL
h <- NULL

skip_on_cran()
skip_on_ci()

result <- loglik(par = par, parms = parms, par_all = par_all,
                 data = data, names = names, Vi = Vi,
                 h = h, u = u, uh = uh, ep = ep, cr = cr)

# 0.
test_that("le résultat est un scalaire numérique fini", {
  expect_length(result, 1)
  expect_true(is.numeric(result))
  expect_true(is.finite(result))
})

# 1.
test_that("le résultat n'est pas NA ou NaN", {
  expect_false(is.na(result))
  expect_false(is.nan(result))
})

# 2.
test_that("deux appels identiques donnent le même résultat", {
  result1 <- loglik(par = par, parms = parms, par_all = par_all,
                    data = data, names = names, Vi = Vi,
                    h = h, u = u, uh = uh, ep = ep, cr = cr)
  expect_equal(result, result1)
})

# 3.
test_that("changer les paramètres change le résultat", {
  result1 <- loglik(par = par * 0.9, parms = parms, par_all = par_all,
                    data = data, names = names, Vi = Vi,
                    h = h, u = u, uh = uh, ep = ep, cr = cr)
  expect_false(isTRUE(all.equal(result, result1)))
})
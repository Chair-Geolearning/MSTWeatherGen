# Libraries:
library(testthat)
library(lubridate)

context("Tests pour la fonction season_indices")

# Data to be tested on
dates_2020 <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="day")  # Année bissextile
dates_2021 <- seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by="day")  # Non bissextile
dates_multi <- c(dates_2020, dates_2021)
season <- list(min_day=1, max_day=30, min_month=6, max_month=6)
winter = list(min_day = 1, max_day = 29, min_month = 12, max_month = 2)

# 1.
test_that("Saison simple dans une seule année", {
  season <- list(min_day=1, max_day=30, min_month=6, max_month=6)
  season_one_day <- list(min_day=1, max_day=1, min_month=6, max_month=6)
  
  idx <- season_indices(dates_2020, season)
  idx_season_one_day <- season_indices(dates_2020, season_one_day)
  
  expect_equal(idx, which(month(dates_2020)==6 ))
  expect_equal(idx_season_one_day, which(month(dates_2020)==6 & (day(dates_2020)==1)))
  
  
})


# 2. 
test_that("Saison chevauchante (décembre -> février)", {
  season <- list(min_day=1, max_day=29, min_month=12, max_month=2)
  
  idx <- season_indices(dates_multi, season)
    # Dates attendues
  expected <- which(
    (month(dates_multi)==12 |
      (month(dates_multi)==1) |
      (month(dates_multi)==2 ))
  )
  
  expect_equal(sort(idx), sort(expected))
})

# 3. 
test_that("Gestion du 29 février en année bissextile et non-bissextile", {
  season <- list(min_day=28, max_day=29, min_month=2, max_month=2)
  
  idx_2020 <- season_indices(dates_2020, season)  # doit inclure le 29
  idx_2021 <- season_indices(dates_2021, season)  # max_day devient 28
  
  expect_true(as.Date("2020-02-29") %in% dates_2020[idx_2020])
  expect_true(as.Date("2021-02-28") %in% dates_2021[idx_2021])
})


# 4. 
test_that("Filtrage par année via l'argument Year", {
  season <- list(min_day=1, max_day=31, min_month=7, max_month=7)
  
  idx_all <- season_indices(dates_multi, season)
  idx_2021 <- season_indices(dates_multi, season, Year=2021)  
  
  expect_true(all(lubridate::year(dates_multi[idx_2021]) == 2021))
  expect_true(length(idx_all) > length(idx_2021))
})


# 5. Test du paramètre Year : Verifier le cas d'hiver.
test_that("du", {

  idx_2021_winter <- season_indices(dates_2021, winter)  
  
  expected <- which(
    (month(dates_2021)==12 |
       (month(dates_2021)==1) |
       (month(dates_2021)==2 ))
  )
  
  expect_equal(sort(idx_2021_winter), sort(expected))
  
})

# 6. Test du paramètre Year : Verifier le cas d'hiver.
test_that("du", {
  idx_2021_winter <- season_indices(dates_2021, winter)  
  
  expected <- which(
    (month(dates_2021)==12 |
       (month(dates_2021)==1) |
       (month(dates_2021)==2 ))
  )
  expect_equal(sort(idx_2021_winter), sort(expected))
  
})



# 7. Cas d'un seul jour.
test_that("Cas d'un seul jour", {
  season_one_day <- list(min_day=1, max_day=1, min_month=6, max_month=6)
  idx_2021_winter <- season_indices(dates_2021, season_one_day)  
  
  expected <- which(
    (month(dates_2021)==6) &  (day(dates_2021)==1)
  )
  expect_equal(sort(idx_2021_winter), sort(expected))
  
})


# 8. Cas d'un winter.
test_that("Cas d'un winter", {
  season_one_day <- list(min_day=1, max_day=1, min_month=6, max_month=6)
  idx_2021_winter <- season_indices(dates_2021, season_one_day)  
  
  expected <- which(
    (month(dates_2021)==6) &  (day(dates_2021)==1)
  )
  expect_equal(sort(idx_2021_winter), sort(expected))
  
})


# 9. Cas d'un winter.
test_that("Cas d'un winter", {
  season_one_day <- list(min_day=1, max_day=1, min_month=6, max_month=6)
  idx_2021_winter <- season_indices(dates_2021, season_one_day)  
  
  expected <- which(
    (month(dates_2021)==6) &  (day(dates_2021)==1)
  )
  expect_equal(sort(idx_2021_winter), sort(expected))
  
})




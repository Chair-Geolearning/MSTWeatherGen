# Libraries:
library(testthat)

# Data Original :
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"), as.Date("2021-12-31"), by = "day")
names_weather_types <- names
seasons <- list(
  s1 = list(min_day = 1, max_day = 29, min_month = 12, max_month = 2),
  s2 = list(min_day = 1, max_day = 31, min_month = 3,  max_month = 5),
  s3 = list(min_day = 1, max_day = 31, min_month = 6,  max_month = 8),
  s4 = list(min_day = 1, max_day = 30, min_month = 9,  max_month = 11)
)

# ── Bivariate Case — With Precipitation ──────────────────────────────────────

names_prec <- c("Precipitation", "Temp_max")
data_prec  <- data[, , names_prec, drop = FALSE]

# ── Bivariate Case — Without Precipitation ───────────────────────────────────

names_no_prec <- c("Wind", "Temp_max")
data_no_prec  <- data[, , names_no_prec, drop = FALSE]

# ── Univariate Cases ──────────────────────────────────────────────────────────

names_univ  <- "Temp_max"
data_univ   <- data[, , 3, drop = FALSE]

names_univ2 <- "Wind"
data_univ2  <- data[, , 2, drop = FALSE]

names_univ_prec <- "Precipitation"
data_univ_prec  <- data[, , 1, drop = FALSE]

# ── Tests ─────────────────────────────────────────────────────────────────────

# ── Trivariate — avec précipitation ───────────────────────────────────────────
test_that("Estim tourne sans erreur — trivarié AVEC précipitation", {
  expect_no_error(
    MSTWeatherGen_Estim(
      data                = data,
      seasons             = seasons,
      dates               = dates,
      names               = names,
      by_season           = TRUE,
      scale               = TRUE,
      precipitation       = TRUE,
      names_weather_types = names_weather_types,
      coordinates         = coordinates,
      max_it              = 100,
      tmax                = 1,
      n1                  = 3,
      n2                  = 3
    )
  )
})

# ── Bivariate — avec précipitation ───────────────────────────────────────────
test_that("Estim tourne sans erreur — bivarié avec précipitation", {
  expect_no_error(
    MSTWeatherGen_Estim(
      data                = data_prec,
      seasons             = seasons,
      dates               = dates,
      names               = names_prec,
      by_season           = TRUE,
      scale               = TRUE,
      precipitation       = TRUE,
      names_weather_types = names_prec,
      coordinates         = coordinates,
      max_it              = 100,
      tmax                = 1,
      n1                  = 3,
      n2                  = 3
    )
  )
})

# ── Bivariate — sans précipitation ───────────────────────────────────────────
test_that("Estim tourne sans erreur — bivarié sans précipitation", {
  expect_no_error(
    MSTWeatherGen_Estim(
      data                = data_no_prec,
      seasons             = seasons,
      dates               = dates,
      names               = names_no_prec,
      by_season           = TRUE,
      scale               = TRUE,
      precipitation       = FALSE,
      names_weather_types = names_no_prec,
      coordinates         = coordinates,
      max_it              = 100,
      tmax                = 1,
      n1                  = 3,
      n2                  = 3
    )
  )
})

# ── Univarié — Wind ───────────────────────────────────────────────────────────
test_that("Estim tourne sans erreur — univarié Wind", {
  expect_no_error(
    MSTWeatherGen_Estim(
      data                = data_univ2,
      seasons             = seasons,
      dates               = dates,
      names               = names_univ2,
      by_season           = TRUE,
      scale               = TRUE,
      precipitation       = FALSE,
      names_weather_types = names_univ2,
      coordinates         = coordinates,
      max_it              = 100,
      tmax                = 1,
      n1                  = 3,
      n2                  = 3
    )
  )
})

# ── Univarié — Precipitation ──────────────────────────────────────────────────
test_that("Estim tourne sans erreur — univarié Precipitation", {
  expect_no_error(
    MSTWeatherGen_Estim(
      data                = data_univ_prec,
      seasons             = seasons,
      dates               = dates,
      names               = names_univ_prec,
      by_season           = TRUE,
      scale               = TRUE,
      precipitation       = TRUE,
      names_weather_types = names_univ_prec,
      coordinates         = coordinates,
      max_it              = 100,
      tmax                = 1,
      n1                  = 3,
      n2                  = 3
    )
  )
})

# ── Univarié — Temp_max ─────────────Probleme des betas negatifs───────────────── A checker avec Jeff
test_that("Estim tourne sans erreur — univarié Temp_max", {
  expect_no_error(
    MSTWeatherGen_Estim(
      data                = data_univ,
      seasons             = seasons,
      dates               = dates,
      names               = names_univ,
      by_season           = TRUE,
      scale               = TRUE,
      precipitation       = FALSE,
      names_weather_types = names_univ,
      coordinates         = coordinates,
      max_it              = 100,
      tmax                = 1,
      n1                  = 3,
      n2                  = 3
    )
  )
})
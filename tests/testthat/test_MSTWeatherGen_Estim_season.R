  # Libraries:
library(testthat)

# Data Original :
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"),as.Date("2021-12-31"), by="day")
names = c("Precipitation", "Wind", "Temp_max")
s1 = list(min_day = 1, max_day = 29, min_month = 12, max_month = 2)

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

# ── Tests ──────────────────────────────────────────────────────────

# ── Trivariate — avec précipitation (variable synthétique) ───────────────────
test_that("Estim_season tourne sans erreur — trivarié AVEC précipitation", {
  expect_no_error(
    MSTWeatherGen_Estim_season(
      data          = data,
      dates         = dates,
      precipitation = TRUE,
      names         = names,
      coordinates   = coordinates,
      season        = s1,
      max_it        = 3,
      tmax          = 2,
      n1            = 3,
      n2            = 3
    )
  )
})

'# ── Trivariate — sans précipitation (variable synthétique) ───────────────────
test_that("Estim_season tourne sans erreur — trivarié sans précipitation", {
  expect_no_error(
    MSTWeatherGen_Estim_season(
      data          = data_triv,
      dates         = dates,
      precipitation = FALSE,
      names         = names_triv,
      coordinates   = coordinates,
      season        = s1,
      max_it        = 3,
      tmax          = 2,
      n1            = 3,
      n2            = 3
    )
  )
})'

# ── Bivariate — avec précipitation ───────────────────────────────────────────
test_that("Estim_season tourne sans erreur — bivarié avec précipitation", {
  expect_no_error(
    MSTWeatherGen_Estim_season(
      data          = data_prec,
      dates         = dates,
      precipitation = TRUE,
      names         = names_prec,
      coordinates   = coordinates,
      season        = s1,
      max_it        = 3,
      tmax          = 2,
      n1            = 3,
      n2            = 3
    )
  )
})

# ── Bivariate — sans précipitation ───────────────────────────────────────────
test_that("Estim_season tourne sans erreur — bivarié sans précipitation", {
  expect_no_error(
    MSTWeatherGen_Estim_season(
      data          = data_no_prec,
      dates         = dates,
      precipitation = FALSE,
      names         = names_no_prec,
      coordinates   = coordinates,
      season        = s1,
      max_it        = 3,
      tmax          = 2,
      n1            = 3,
      n2            = 3
    )
  )
})


# ── Univarié — Wind ───────────────────────────────────────────────────────────
test_that("Estim_season tourne sans erreur — univarié Wind", {
  expect_no_error(
    MSTWeatherGen_Estim_season(
      data          = data_univ2,
      dates         = dates,
      precipitation = FALSE,
      names         = names_univ2,
      coordinates   = coordinates,
      season        = s1,
      max_it        = 3,
      tmax          = 2,
      n1            = 3,
      n2            = 3
    )
  )
})

# ── Univarié — Temp_max ─────────────────────────────────────────────────────── A checker avec Jeff
'test_that("Estim_season tourne sans erreur — univarié Temp_max", {
  expect_no_error(
    MSTWeatherGen_Estim_season(
      data          = data_univ,
      dates         = dates,
      precipitation = FALSE,
      names         = names_univ,
      coordinates   = coordinates,
      season        = s1,
      max_it        = 3,
      tmax          = 2,
      n1            = 3,
      n2            = 3
    )
  )
})'

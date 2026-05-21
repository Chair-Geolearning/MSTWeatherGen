  # Libraries:
library(testthat)

# Data :
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"),as.Date("2021-12-31"), by="day")
names = c("Precipitation", "Wind", "Temp_max")
s1 = list(min_day = 1, max_day = 29, min_month = 12, max_month = 2)

# Data without precipitation :
names_no_prec <- c("Wind", "Temp_max")
data_no_prec  <- data[, , 2:3, drop = FALSE]

test_that("Estim_season tourne sans erreur sans Precipitation", {
  expect_no_error(
    MSTWeatherGen_Estim_season(
      data                = data_no_prec,
      dates               = dates,
      precipitation       = FALSE,
      names               = names_no_prec,
      coordinates         = coordinates,
      season              = s1,  
      max_it              = 3,       
      tmax                = 2,
      n1                  = 3,
      n2                  = 3
    )
  )
})

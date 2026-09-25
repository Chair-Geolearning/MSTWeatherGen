library(MSTWeatherGen)
library(testthat)

names = c("Precipitation", "Wind", "Temp_max")
par_all <- c()

par_all["Precipitation-Precipitation:rho2ij"] <- 1.0
par_all["Wind-Wind:rho2ij"] <- 1.0
par_all["Temp_max-Temp_max:rho2ij"] <- 1.0
par_all["Precipitation-Wind:rho2ij"]<- 0.5
par_all["Precipitation-Temp_max:rho2ij"] <- 0.5
par_all["Wind-Temp_max:rho2ij"] <- 0.5
  
a_matrix <- matrix(
  c(
    0.4, 0.6, 0.7,
    0.6, 0.3, 0.8,
    0.7, 0.8, 0.1
  ),
  nrow = 3, ncol = 3,
  dimnames = list(names, names)
)

test_that("update_rho2_parameters runs without error with matrix input", {

  expect_silent({
    result <- update_rho2_parameters(par_all, names, a_matrix)
  })

  expect_equal(class(result), class(par_all))
  expect_type(result, "double")
  expect_false(anyNA(result))
  expect_equal(length(result), length(par_all))

  expect_equal(unname(result["Precipitation-Precipitation:rho2ij"]), 0.4)
  expect_equal(unname(result["Wind-Wind:rho2ij"]), 0.3)
  expect_equal(unname(result["Temp_max-Temp_max:rho2ij"]), 0.1)
  expect_equal(unname(result["Precipitation-Wind:rho2ij"]), 0.6)
  expect_equal(unname(result["Wind-Temp_max:rho2ij"]), 0.8)
  expect_equal(unname(result["Precipitation-Temp_max:rho2ij"]), 0.7)
})

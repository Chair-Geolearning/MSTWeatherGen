# Libraries:
library(testthat)
library(PTAk)
library(MSTWeatherGen)  

# Data to be tested on:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2012-01-01"),as.Date("2022-12-31"), by="day")
rescompressed <- data_compression(data)



# 1. 
test_that("La fonction renvoie une matrice non vide", {
  expect_false(is.null(rescompressed))
  expect_true(is.matrix(rescompressed))
  expect_gt(nrow(rescompressed), 0)
  expect_gt(ncol(rescompressed), 0)
})

# 2. 
test_that("La sortie contient au maximum 4 colonnes et pas de duplicats", {
  expect_lte(ncol(rescompressed), 4)
  expect_equal(ncol(rescompressed), ncol(unique(res, MARGIN = 2)))
})

# 3.  
test_that("Toutes les valeurs de sortie sont finies", {
  expect_false(any(!is.finite(rescompressed)))
})


# 4.
test_that("Fonction gère un sous-échantillon", {
  sub_data <- data[,1,, drop = FALSE]  # une station seulement
  res_sub <- data_compression(sub_data)
  
  expect_true(is.matrix(res_sub))
  expect_lte(ncol(res_sub), 4)
})

# 5.
test_that("Fonction gère une seule variable", {
  sub_data <- data[,,1, drop = FALSE]  # première variable
  res_sub <- data_compression(sub_data)
  expect_true(is.matrix(res_sub))
  expect_lte(ncol(res_sub), 4)
})

library(testthat)
library(MSTWeatherGen)

# Chargement des données du package
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
par <- rep(1, 26)

names <- c("Precipitation", "Wind", "Temp_max")

test_that("compute_beta returns a square symmetric matrix with correct names", {
  
  # Matrice de corrélation empirique simple (valide)
  cr <- diag(1, length(names))
  colnames(cr) <- rownames(cr) <- names
  
  parm <- par
  
  beta <- compute_beta(parm, names, cr)
  
  expect_true(is.matrix(beta))
  expect_equal(dim(beta), c(length(names), length(names)))
  expect_equal(colnames(beta), names)
  expect_equal(rownames(beta), names)
  expect_equal(beta, t(beta))
})

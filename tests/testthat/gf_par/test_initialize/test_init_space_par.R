# Libraries:
library(testthat)
library(MSTWeatherGen)  

# Data:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"),as.Date("2021-12-31"), by="day")
names = c("Precipitation", "Wind", "Temp_max")

h <- as.vector(dist(coordinates))

# uh = matrice additionnelle (exemple simple pour test)
uh <- matrix(1, nrow = length(h), ncol = length(h))  

test_that("init_space_par fonctionne sur données réelles", {
  
  result <- init_space_par(data, names, h, uh, max_it = 50)
  
  # Vérifier que la liste a le bon nombre d'éléments
  expect_equal(length(result), length(names))
  
  # Vérifier que chaque élément est un vecteur numérique de taille 2
  for (res in result) {
    expect_true(is.numeric(res))
    expect_equal(length(res), 2)
  }
})

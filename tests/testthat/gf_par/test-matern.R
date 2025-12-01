library(testthat)

# Assurez-vous que la fonction Matern est chargée
# source("R/Matern.R")  # décommentez si nécessaire

test_that("Matern renvoie bien des valeurs correctes", {
  
  # 1️⃣ Cas de base : h = 0
  h0 <- 0
  expect_equal(Matern(h0, r = 1, v = 0.5), 1, tolerance = 1e-8)
  
  # 2️⃣ Cas général : vecteur de distances
  h_vec <- c(0, 0.5, 1, 2)
  result <- Matern(h_vec, r = 1, v = 0.5)
  
  expect_length(result, length(h_vec))        # même longueur que h_vec
  expect_true(all(result <= 1 & result >= 0)) # covariances entre 0 et 1
  
  # 3️⃣ Valeurs précises pour vérifier la formule
  expect_equal(round(result[2], 4), 0.7788)
  expect_equal(round(result[3], 4), 0.6065)
  
  # 4️⃣ Différents paramètres de lissage v
  result_v1 <- Matern(h_vec, r = 1, v = 1.5)
  expect_true(all(result_v1 <= 1 & result_v1 >= 0))
  
  # 5️⃣ Grandes distances
  h_large <- 10
  expect_true(Matern(h_large, r = 1, v = 0.5) < 1e-3)
})

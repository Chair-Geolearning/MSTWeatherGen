# Libraries:
library(testthat)

EPS_TOL <- 1e-10 
# ── Tests ─────────────────────────────────────────────────────────────────────

test_that("matrice définie positive simple retourne TRUE", {
  M <- diag(3)                        # valeurs propres = 1, 1, 1
  expect_true(is_positive_definite(M))
})

test_that("matrice 2x2 définie positive retourne TRUE", {
  M <- matrix(c(4, 2, 2, 3), nrow = 2)
  expect_true(is_positive_definite(M))
})

test_that("matrice semi-définie positive retourne FALSE (valeur propre = 0)", {
  # Rang 1 : valeurs propres = 2, 0
  M <- matrix(c(1, 1, 1, 1), nrow = 2)
  expect_false(is_positive_definite(M))
})

test_that("matrice non-définie retourne FALSE (valeur propre négative)", {
  M <- matrix(c(1, 2, 2, 1), nrow = 2)   # valeurs propres = 3, -1
  expect_false(is_positive_definite(M))
})

test_that("matrice définie négative retourne FALSE", {
  M <- -diag(3)                           # valeurs propres = -1, -1, -1
  expect_false(is_positive_definite(M))
})

test_that("la variable tol personnalisée accepte une valeur propre marginale", {
  # Matrice avec valeur propre ≈ 0.5e-6 (en dessous de tol par défaut 1e-6)
  M <- diag(c(1, 0.5e-6))
  expect_false(is_positive_definite(M))                      # tol = 1e-6
  expect_true(is_positive_definite(M, tol = 1e-7))           # tol plus souple
})

test_that("tol = 0 accepte une matrice semi-définie positive", {
  M <- matrix(c(1, 1, 1, 1), nrow = 2)   # valeur propre = 0
  expect_true(is_positive_definite(M, tol = 0))
})

test_that("matrice 1x1 positive retourne TRUE", {
  expect_true(is_positive_definite(matrix(5)))
})

test_that("matrice 1x1 nulle retourne FALSE", {
  expect_false(is_positive_definite(matrix(0)))
})

test_that("matrice 1x1 negative retourne FALSE", {
  expect_false(is_positive_definite(matrix(-5)))
})

test_that("grande matrice d'identité retourne TRUE", {
  M <- diag(100)
  expect_true(is_positive_definite(M))
})

test_that("matrice de covariance réelle retourne TRUE", {
  X  <- matrix(rnorm(300), nrow = 100, ncol = 3)
  M  <- crossprod(X) / 99          # t(X) %*% X / (n-1), toujours DPD
  expect_true(is_positive_definite(M))
})



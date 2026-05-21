# Libraries:
library(testthat)

# ── Tests ─────────────────────────────────────────────────────────────────────

# 1.
test_that("La fonction ne retourne pas d'erreur sur une entree valide", {
  M <- matrix(c(4, 2, 2, 3), 2, 2)
  expect_no_error(make_positive_definite(M))
})

# 2.
test_that("La sortie est une matrice numerique", {
  R <- make_positive_definite(diag(3))
  expect_true(is.matrix(R))
  expect_true(is.numeric(R))
})

# 3.
test_that("Les dimensions de la sortie correspondent a celles de l'entree", {
  for (n in c(2, 5, 10)) {
    A <- matrix(rnorm(n * n), n, n)
    M <- A + t(A)
    R <- make_positive_definite(M)
    expect_equal(dim(R), c(n, n))
  }
})

# 4.
test_that("Une matrice identite est laissee inchangee", {
  M <- diag(3)
  R <- make_positive_definite(M)
  expect_equal(R, M, tolerance = 1e-10)
})

# 5.
test_that("La sortie est symetrique", {
  set.seed(42)
  A <- matrix(rnorm(25), 5, 5)
  M <- A + t(A)
  R <- make_positive_definite(M)
  expect_true(is_symmetric(R))
})

# 6.
test_that("Une matrice deja definie positive reste definie positive", {
  M <- matrix(c(4, 2, 2, 3), 2, 2)
  R <- make_positive_definite(M)
  expect_true(min_eigenvalue(R) >= EPS_TOL)
})

# 7.
test_that("Une matrice avec une valeur propre negative devient definie positive", {
  V <- matrix(c(1, 1, 1, -1) / sqrt(2), 2, 2)
  M <- V %*% diag(c(2, -0.5)) %*% t(V)   # lambda_2 = -0.5
  expect_false(is_positive_definite(M))
  R <- make_positive_definite(M)
  expect_true(min_eigenvalue(R) >= EPS_TOL)
})

# 8.
test_that("Une matrice singuliere (valeur propre nulle) devient definie positive", {
  M <- matrix(c(1, 1, 1, 1), 2, 2)       # det = 0, rang 1
  expect_false(is_positive_definite(M))
  R <- make_positive_definite(M)
  expect_true(min_eigenvalue(R) >= EPS_TOL)
})

# 9.
test_that("Le plancher epsilon est applique avec precision flottante", {
  # VP ≈ [+2.41, −0.41] ; apres correction ≈ [2.41, 1e-6]
  # La reconstruction peut introduire une erreur relative de ~1.5e-9
  M <- matrix(c(2, 1, 1, 0), 2, 2)
  R <- make_positive_definite(M)
  expect_true(min_eigenvalue(R) >= EPS_TOL)
})

# 10.
test_that("Un epsilon personnalise est respecte", {
  M   <- matrix(c(1, 0, 0, 0), 2, 2)
  eps <- 0.01
  R   <- make_positive_definite(M, epsilon = eps)
  expect_true(min_eigenvalue(R) >= eps * (1 - 1e-8))
})

# 11.
test_that("Les grandes valeurs propres ne sont pas modifiees", {
  M  <- diag(c(100, 200, 300))
  R  <- make_positive_definite(M)
  ev <- sort(eigen(R, symmetric = TRUE, only.values = TRUE)$values,
             decreasing = TRUE)
  expect_equal(ev, c(300, 200, 100), tolerance = 1e-8)
})

# 12.
test_that("Une matrice entierement negative est corrigee : toutes VP = epsilon", {
  M  <- -diag(3)
  R  <- make_positive_definite(M)
  ev <- eigen(R, symmetric = TRUE, only.values = TRUE)$values
  expect_true(all(abs(ev - 1e-6) < 1e-12))
})

# 13.
test_that("Une matrice quasi-DP subit une correction minimale", {
  set.seed(99)
  M <- diag(3) + 1e-8 * matrix(rnorm(9), 3, 3)
  M <- (M + t(M)) / 2
  R <- make_positive_definite(M)
  expect_lt(frobenius_dist(R, diag(3)), 0.01)
})

# 14.
test_that("La fonction est idempotente : f(f(M)) == f(M)", {
  set.seed(7)
  A  <- matrix(rnorm(16), 4, 4)
  M  <- A + t(A)
  R1 <- make_positive_definite(M)
  R2 <- make_positive_definite(R1)
  expect_equal(R1, R2, tolerance = 1e-10)
})

# 15. [BUG CONNU]
test_that("Une matrice 1x1 provoque une erreur (bug diag(scalaire) en R)", {
  # diag(x) avec x scalaire cree une matrice de taille x, pas [[x]].
  # Fix : diag(corrected_values, nrow = length(corrected_values))
  M <- matrix(-3, 1, 1)
  expect_error(make_positive_definite(M))
})

# 16.
test_that("50 matrices symetriques aleatoires sont toutes corrigees en DP", {
  set.seed(123)
  for (i in seq_len(50)) {
    n  <- sample(2:8, 1)
    A  <- matrix(rnorm(n * n), n, n)
    M  <- A + t(A)
    R  <- make_positive_definite(M)
    ev <- eigen(R, symmetric = TRUE, only.values = TRUE)$values
    expect_true(
      all(ev >= EPS_TOL),
      label = sprintf("iteration %d, n = %d, min VP = %.3e", i, n, min(ev))
    )
  }
})


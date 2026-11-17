# Libraries:
library(testthat)

# Data:
data("data", package = "MSTWeatherGen")
names <- c("Precipitation", "Wind", "Temp_max")

# --- Tests ---

# 1.
test_that("process_empirical_correlation returns a matrix named by variables", {
  cr <- process_empirical_correlation(data, names)
  expect_true(is.matrix(cr))
  expect_equal(dim(cr), c(3, 3))
  expect_equal(rownames(cr), names)
  expect_equal(colnames(cr), names)
})

# 2.
test_that("the correlation matrix is symmetric", {
  cr <- process_empirical_correlation(data, names)
  # Pearson correlation is symmetric -> cr[i,j] == cr[j,i] exactly
  expect_equal(cr, t(cr))
})

# 3.
test_that("correlations are bounded in [-1, 1] and the diagonal is 1", {
  cr <- process_empirical_correlation(data, names)
  expect_true(all(cr >= -1 - 1e-12 & cr <= 1 + 1e-12))
  expect_equal(unname(diag(cr)), rep(1, length(names)))
})

# 4.
test_that("process_empirical_correlation matches the manual site-averaged Pearson cor", {
  cr <- process_empirical_correlation(data, names)

  # manual: mean over sites of cor(Zi, Zj) at the same site
  cr_manual <- sapply(names, function(v1) {
    sapply(names, function(v2) {
      mean(sapply(1:dim(data)[2], function(j)
        cor(data[, j, v1], data[, j, v2], use = "complete.obs")),
        na.rm = TRUE)
    })
  })
  cr_manual <- matrix(cr_manual, nrow = length(names), ncol = length(names),
                      dimnames = list(names, names))
  expect_equal(cr, cr_manual)
})

# 5.
test_that("process_empirical_correlation handles a subset of variables", {
  cr <- process_empirical_correlation(data, c("Precipitation", "Wind"))
  expect_equal(dim(cr), c(2, 2))
  expect_equal(rownames(cr), c("Precipitation", "Wind"))
})

# 6.
test_that("independent variables give near-zero correlation (synthetic check)", {
  set.seed(867)
  # build a synthetic 2-variable array with independent series
  n_t <- 500
  n_s <- 3
  syn <- array(NA, dim = c(n_t, n_s, 2), dimnames = list(NULL, NULL, c("a", "b")))
  syn[, , 1] <- rnorm(n_t * n_s)
  syn[, , 2] <- rnorm(n_t * n_s)
  cr <- process_empirical_correlation(syn, c("a", "b"))
  expect_true(is.matrix(cr))
  expect_true(dim(cr)[1] == 2)
  # with independence the correlation should be near 0 (loose tolerance)
  expect_lt(abs(cr[1, 2]), 0.2)
})

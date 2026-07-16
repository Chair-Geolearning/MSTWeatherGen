# Libraries:
library(testthat)

# Data:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names <- c("Precipitation", "Wind", "Temp_max")
dates <- seq(as.Date("2018-01-01"), as.Date("2021-12-31"), by = "day")
names <- c("Precipitation", "Wind", "Temp_max")

names_univ  <- "Temp_max"
data_univ   <- data[, , 3, drop = FALSE]

# Dimensions
Nt <- dim(data)[1]
Ns <- dim(data)[2]
Nv <- dim(data)[3]

# Parameters
ax <- ax_file
wt <- resultperm$cluster
K <- length(unique(wt))
par_s <- do.call(cbind, par_s)
ep <- generate_variable_index_pairs(names)
pairs <- paste(ep[, 1], ep[, 2], sep = "-")

cr <- sapply(names, function(v1) {
  sapply(names, function(v2) {
    mean(sapply(1:dim(data)[2], function(j) cor(data[, j, v1], data[, j, v2], use = "complete.obs")), na.rm = TRUE)
  })
})

par_all_TEST <- initialize_par_all_if_missing(
  par_all = NULL,
  names = names,
  pairs = pairs,
  par_s = par_s,
  beta1 = ax,
  cr = cr
)
par_all_TEST_updated <- update_beta1_parameters(par_all_TEST, names, extract_beta1(param(par_all_TEST, names), names))
parm <- param(par_all_TEST_updated, names)

parm_univ <- parm[parm$v1 == names_univ & parm$v2 == names_univ, ]

#--------Tests--------

# 0.
test_that("extract_beta1 runs without error", {
  expect_silent({
    result <- extract_beta1(parm, names)
  })
  expect_true(is.matrix(result))
  expect_false(anyNA(result))
})

# 1.
test_that("extract_beta1 returns correct matrix dimensions", {
  result <- extract_beta1(parm, names)

  expect_equal(nrow(result), length(names))
  expect_equal(ncol(result), length(names))
  expect_equal(dim(result), c(3, 3))
})

# 2.
test_that("extract_beta1 returns matrix with correct names", {
  result <- extract_beta1(parm, names)

  expect_equal(colnames(result), names)
  expect_equal(rownames(result), names)
})

# 3.
test_that("matrix is symmetric", {
  result <- extract_beta1(parm, names)

  expect_true(isSymmetric(result))
  expect_true(all(is.finite(result)))
})

# 4.
test_that("function is deterministic", {
  result1 <- extract_beta1(parm, names)
  result2 <- extract_beta1(parm, names)

  expect_identical(result1, result2)
})

#5.
test_that("extract_beta1 univarié (length(names)==1) retourne une matrice", {
  result <- extract_beta1(parm_univ, names_univ)
  expect_true(is.matrix(result))
  expect_equal(dim(result), c(1L, 1L))
  expect_equal(rownames(result), names_univ)
  expect_equal(colnames(result), names_univ)
  expect_false(anyNA(result))
})

#6.
test_that("extract_beta1 univarié : la valeur est préservée", {
  result <- extract_beta1(parm_univ, names_univ)
  expect_equal(result[1, 1], parm_univ$beta1ij)
})

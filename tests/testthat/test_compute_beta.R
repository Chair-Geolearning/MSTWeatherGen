library(testthat)

# Data
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names <- c("Precipitation", "Wind", "Temp_max")
ep <- generate_variable_index_pairs(names)

# Parameters
par_s <- readRDS("saved_results/pars.rds")
par_s <- do.call(cbind, par_s)
variable = 'Wind'
pairs <- c("Wind-Wind", "Wind-Temp", "Temp-Temp")
par_all_TEST.rds <- readRDS("saved_results/par_all_TEST.rds")
parm <- param(par_all_TEST.rds, names)


cr <- sapply(names, function(v1) {
  sapply(names, function(v2) {
    mean(sapply(1:dim(data)[2], function(j) cor(data[, j, v1], data[, j, v2], use = "complete.obs")), na.rm = TRUE)
  })
})

# 0.
test_that("compute_beta returns a symmetric matrix", {
  result <- compute_beta(parm, names, cr)
  
  expect_true(is.matrix(result))
  expect_equal(nrow(result), ncol(result))
  expect_equal(result, t(result))
})

# 1.
test_that("compute_beta dimensions", {
  result <- compute_beta(parm, names, cr)
  
  expect_equal(nrow(result), length(names))
  expect_equal(ncol(result), length(names))
})

# 2.
test_that("compute_beta names", {
  result <- compute_beta(parm, names, cr)
  
  expect_equal(rownames(result), names)
  expect_equal(colnames(result), names)
})

# 3.
test_that("compute_beta retourne des valeurs numériques", {
  result <- compute_beta(parm, names, cr)
  
  expect_true(all(is.numeric(result)))
  expect_true(all(is.finite(result)))
})


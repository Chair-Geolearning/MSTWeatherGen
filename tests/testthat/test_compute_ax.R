# Libraries:
library(testthat)

# Data:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"),as.Date("2021-12-31"), by="day")
names = c("Precipitation", "Wind", "Temp_max")

# Dimensions
Nt <- dim(data)[1]
Ns <- dim(data)[2]
Nv <- dim(data)[3]

# Parameters
resultperm <- readRDS("saved_results/resultperm2.rds")
ax <- readRDS("saved_results/ax_file.rds")
wt <- resultperm$cluster
K <- length(unique(wt))
par_s <- readRDS("saved_results/pars.rds")
par_s <- do.call(cbind, par_s)
ep <- generate_variable_index_pairs(names)
pairs <- paste(ep[,1],ep[,2], sep = "-")

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
  ax = ax,
  cr = cr
)
par_all_TEST_updated <- update_ax_parameters(par_all_TEST, names, compute_ax(param(par_all_TEST, names), names))
parm <- param(par_all_TEST_updated, names)

#--------Tests--------

#0.
test_that("compute_ax runs without error", {

  expect_silent({
    result <- compute_ax(parm, names)
  })
  expect_true(is.matrix(result))
  expect_false(anyNA(result))
})

#1.
test_that("compute_ax returns correct matrix dimensions", {
  result <- compute_ax(parm, names)
  
  expect_equal(nrow(result), length(names))
  expect_equal(ncol(result), length(names))
  expect_equal(dim(result), c(3, 3))
})

#2.
test_that("compute_ax returns matrix with correct names", {
  result <- compute_ax(parm, names)
  
  expect_equal(colnames(result), names)
  expect_equal(rownames(result), names)
})

#3.
test_that("matrix is symmetric", {
  result <- compute_ax(parm, names)

  expect_true(isSymmetric(result))
  expect_true(all(is.finite(result)))
  
})

#4.
test_that("function is deterministic", {

  result1 <- compute_ax(parm, names)
  result2 <- compute_ax(parm, names)
  
  expect_identical(result1, result2)
})


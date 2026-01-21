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
ax <- ax_file
wt <- resultperm$cluster
K <- length(unique(wt))
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


#--------Tests--------

#0.
test_that("update_ax_parameters runs without error with matrix input", {

  ax_matrix <- matrix(c(1, 0.5, 0.3,
                        0.5, 1, 0.4,
                        0.3, 0.4, 1), 
                      nrow = 3, ncol = 3,
                      dimnames = list(names, names))
  
  expect_silent({
    result <- update_ax_parameters(par_all_TEST, names, ax_matrix)
  })
  
  expect_type(result, "double")
  expect_false(anyNA(result))
  expect_equal(length(result), length(par_all_TEST))
})

#1.
test_that("matrix input values are correctly assigned", {

  ax_matrix <- matrix(c(1.0, 0.5, 0.5,
                        0.5, 1.0, 0.5,
                        0.5, 0.5, 1.0), 
                      nrow = 3, ncol = 3)
  
  result <- update_ax_parameters(par_all_TEST, names, ax_matrix)
  
  expect_equal( unname(result["Precipitation-Precipitation:ax"]), 1.0)
  expect_equal( unname(result["Wind-Wind:ax"]), 1.0)
  expect_equal( unname(result["Temp_max-Temp_max:ax"]), 1.0)
  
  expect_equal( unname(result["Precipitation-Wind:ax"]), 0.5)
  expect_equal( unname(result["Wind-Temp_max:ax"]), 0.5)
  expect_equal( unname(result["Precipitation-Temp_max:ax"]), 0.5)
                              
  expect_equal( unname(result["Wind-Precipitation:ax"]), 0.5)
  expect_equal( unname(result["Temp_max-Wind:ax"]), 0.5)
  expect_equal( unname(result["Temp_max-Precipitation:ax"]), 0.5)
})

#2.
test_that("nearPD is applied to non-positive definite matrix", {

  ax_matrix_non_PD <- matrix(c(1, 2, 0,
                        2, 1, 0,
                        0, 0, 1), 
                      nrow = 3, ncol = 3)
  
  result <- update_ax_parameters(par_all_TEST, names, ax_matrix_non_PD)
  
  expect_type(result, "double")
  expect_false(anyNA(result))

})

#3.
test_that("all zeros matrix (extreme case)", {

  ax_zeros <- matrix(0, nrow = 3, ncol = 3)
  
  expect_error(result <- update_ax_parameters(par_all_TEST, names, ax_zeros))
  
})

#4.
test_that("output preserves par_all structure", {

  ax_matrix <- diag(3)
  
  result <- update_ax_parameters(par_all_TEST, names, ax_matrix)
  
  expect_equal(length(result), length(par_all_TEST))
  expect_equal(names(result), names(par_all_TEST))
  expect_type(result, typeof(par_all_TEST))
  
})

#5.
test_that("function is deterministic with same inputs", {

  ax_matrix <- matrix(c(1, 0.5, 0.3,
                        0.5, 1, 0.4,
                        0.3, 0.4, 1), 
                      nrow = 3, ncol = 3)
  
  result1 <- update_ax_parameters(par_all_TEST, names, ax_matrix)
  result2 <- update_ax_parameters(par_all_TEST, names, ax_matrix)
  
  expect_identical(result1, result2)
})


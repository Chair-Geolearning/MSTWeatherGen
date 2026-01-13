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
resultperm <- readRDS(testthat::test_path("saved_results/resultperm2.rds"))
ax <- readRDS(testthat::test_path("saved_results/ax_file.rds"))
wt <- resultperm$cluster
K <- length(unique(wt))
par_s <- readRDS(testthat::test_path("saved_results/pars.rds"))
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


#--------Tests--------

# 0.
test_that("initialize_par_all_if_missing runs without error", {
  
  expect_silent({
    par_all <- initialize_par_all_if_missing(
      par_all = NULL,
      names = names,
      pairs = pairs,
      par_s = par_s,
      ax = ax,
      cr = cr
    )
  })
  
  expect_type(par_all, "double")
  expect_false(anyNA(par_all))
  expect_true(length(par_all) > 0)
  expect_true(all(is.finite(par_all)))
  
})

# 1.
test_that("initialize_par_all_if_missing creates expected parameter names", {
  
  expected_names <- c(
    paste(pairs, "dij", sep=":"),
    paste(pairs, "rij", sep=":"),
    paste(pairs, "vij", sep=":"),
    paste(pairs, "ax",  sep=":")
  )
  
  expect_true(all(expected_names %in% names(par_all_TEST)))
})

# 2.
test_that("existing par_all is not overwritten", {
  
  par_all_init <- par_all_TEST
  
  par_all <- initialize_par_all_if_missing(
    par_all = NULL,
    names = names,
    pairs = pairs,
    par_s = par_s,
    ax = ax,
    cr = cr
  )
  
  par_all_bis <- initialize_par_all_if_missing(
    par_all = par_all,
    names = names,
    pairs = pairs,
    par_s = par_s,
    ax = ax,
    cr = cr
  )
  
  expect_equal(par_all_bis[names(par_all)], par_all)
})

# 3. 
test_that("default values are correctly assigned", {
  
  dij_params <- paste(pairs, "dij", sep = ":")
  expect_true(all(par_all_TEST[dij_params] == 1))
  
  parms <- c("a1", "a2", "d1", "d2", "g1", "g2")
  expect_true(all(par_all_TEST[parms] == 1))
  
  other_params <- c("b1", "e1", "l1", "b2", "e2", "l2", "c", "f", "m")
  expect_true(all(par_all_TEST[other_params] == 0.1))
})

# 4. 
test_that("par_s values are correctly assigned to rij and vij", {
  
  rij_params <- paste(pairs[1:length(names)], "rij", sep = ":")
  vij_params <- paste(pairs[1:length(names)], "vij", sep = ":")
  
  expect_equal(unname(par_all_TEST[rij_params]), par_s[1,])
  expect_equal(unname(par_all_TEST[vij_params]), par_s[2,])
})

# 5. 
test_that("ax parameters are initialized to 0 then updated", {
  ax_params <- paste(pairs, "ax", sep = ":")
  
  expect_true(all(ax_params %in% names(par_all_TEST)))
  expect_type(par_all_TEST[ax_params], "double")
})

# 7. 
test_that("par_all has expected length", {
  n_pairs <- length(pairs)
  n_names <- length(names)
  
  expected_length <- n_pairs * 4 +  
    15 +             
    n_names * 3+3      
  expect_equal(length(par_all_TEST), expected_length)
})

# 8. 
'test_that("function handles single variable case", {
  
  single_name <- names[1]
  single_pairs <- pairs[1]
  single_par_s <- matrix(par_s[, 1], ncol = 1)
  
  par_all_single <- initialize_par_all_if_missing(
    par_all = NULL,
    names = single_name,
    pairs = single_pairs,
    par_s = single_par_s,
    ax = ax[1,],
    cr = cr[1, 1, drop = FALSE]
  )
  
  expect_type(par_all_single, "double")
  expect_true(length(par_all_single) > 0)
})'

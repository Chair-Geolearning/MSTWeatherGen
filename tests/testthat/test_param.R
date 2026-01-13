library(testthat)

# Data
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names <- c("Precipitation", "Wind", "Temp_max")
ep <- generate_variable_index_pairs(names)


par_all_TEST <- readRDS(testthat::test_path("saved_results/par_all_TEST.rds"))
names <- c("Precipitation", "Wind", "Temp_max")

# ------------ TESTS --------------

# 0.
test_that("param return a df", {
  parm <- param(par_all_TEST, names)
  
  expect_true(is.data.frame(parm))
})

# 1.
test_that("col and row number", {
  parm <- param(par_all_TEST, names)
  
  expect_equal(nrow(parm), 6)
})

# 2.
test_that("the result contain v1 and v2", {
  parm <- param(par_all_TEST, names)
  
  expect_true("v1" %in% colnames(parm))
  expect_true("v2" %in% colnames(parm))
})

# 3.
test_that("it contains the right parameters.", {
  parm <- param(par_all_TEST, names)
  
  required_cols <- c("v1", "v2", "a1", "d1", "g1", "a2", "d2", "g2",
                     "b1", "e1", "l1", "b2", "e2", "l2", "c", "f", "m",
                     "ai", "aj", "bi", "bj", "ci", "cj", 
                     "rii", "rjj", "vii", "vjj", "ax", "dij")
  
  expect_true(all(required_cols %in% colnames(parm)))
})


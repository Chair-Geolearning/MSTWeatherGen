library(testthat)

# Data
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names <- c("Precipitation", "Wind", "Temp_max")
ep <- generate_variable_index_pairs(names)


par_all_TEST <- par_all_TEST.rds
names <- c("Precipitation", "Wind", "Temp_max")

# ------------ TESTS --------------

# 0.
test_that("param return a df", {
  parm <- create_df_param(par_all_TEST, names)

  expect_true(is.data.frame(parm))
})

# 1.
test_that("col and row number", {
  parm <- create_df_param(par_all_TEST, names)

  expect_equal(nrow(parm), 6)
})

# 2.
test_that("the result contain v1 and v2", {
  parm <- create_df_param(par_all_TEST, names)

  expect_true("v1" %in% colnames(parm))
  expect_true("v2" %in% colnames(parm))
})

# 3.
test_that("it contains the right parameters.", {
  parm <- create_df_param(par_all_TEST, names)

  required_cols <- c(
    "v1", "v2",
    "a", "b", "c", "d", "e",
    "Ai", "Aj",
    "aii", "ajj", "nuii", "nujj",
    "rho1ij",
    "r2ii", "r2jj", "r1ii", "r1jj",
    "rho2ij"
  )

  expect_true(all(required_cols %in% colnames(parm)))
})

# Libraries:
library(testthat)
library(PTAk)

# Data:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2012-01-01"),as.Date("2022-12-31"), by="day")
K <- 5

# Retrieving results:
wt <- resultperm$cluster

# ----- Test of the sub-function estimtransition -----
M <- estimtransition(wt, dates, K)

# 0.
test_that("Structure of the results", {

  expect_true(is.matrix(M))
  expect_equal(dim(M), c(K, K))
})

# 1.
test_that("The same experiment produces the same results", {
  M1 <- estimtransition(wt, dates, K)

  expect_equal(M, M1)
})

# 2.
test_that("Rows with no outgoing transitions remain zero", {
  wt_dummy <- c(1, 1, 1, 1)  # Only 1 appears
  dates_dummy <- as.Date("2020-01-01") + 0:3
  K <- 4   # But we say there are 4 possible states
  M <- estimtransition(wt_dummy, dates_dummy, K)

  
  expect_equal(M[1,], c(1,0,0,0)) # all transitions go from 1 to 1
  expect_true(all(is.na(M[2:4,])))
})

# 3.
test_that("Transitions are counted separately per year", {
  wt_years <- c(1,2,  2,3)  # should produce two transitions: 1->2 in year1, and 2->3 in year2
  dates_years <- as.Date(c("2020-01-01","2020-01-02", "2021-01-01","2021-01-02"))
  K <- 3
  M <- estimtransition(wt_years, dates_years, K)
  
  expect_equal(M[1,2], 1) # from year 2020
  expect_equal(M[2,3], 1) # from year 2021
})

# 4.
test_that("Rows with transitions sum to 1 on realistic data", {
  row_sums <- rowSums(M, na.rm = TRUE)

  expect_true(all(row_sums[row_sums > 0] == 1))
  expect_true(all(M <= 1 | is.na(M)))  
})

# ----- TEST of the function estimate_transitions -----

# 1.
'test_that("estimate_transitions output matrices contain no NA", {
  tm <- estimate_transitions(wt, dates, nb = 5, K = 5)
  
  lapply(tm, function(M) {
    expect_true(anyNA(M))
  })
})'

# 2.
test_that("estimate_transitions returns a list", {
  
  tm <- estimate_transitions(wt, dates, nb = 10, K = 5)
  expect_type(tm, "list")
  expect_length(tm, length(wt))
})

# 3.
test_that("Same experiment leads to same results", {
  set.seed(1)
  tm1 <- estimate_transitions(wt, dates, nb = 10, K = 5)
  set.seed(1)
  tm2 <- estimate_transitions(wt, dates, nb = 10, K = 5)
  
  expect_equal(tm1,tm2)
  
})

# 4.
'test_that("estimate_transitions output values lie between 0 and 1", {
  tm <- estimate_transitions(wt, dates, nb = 5, K = 5)
  
  for (M in tm) {
    expect_true(all(M >= 0))
    expect_true(all(M <= 1))
  }
})'


# Libraries:
library(testthat)
library(MSTWeatherGen)  

# Data:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"),as.Date("2021-12-31"), by="day")
names = c("Precipitation", "Wind", "Temp_max")

resultperm <- readRDS("resultperm2.rds")

set.seed(1)
wt <- resultperm$cluster
K <- length(unique(wt))

# Récupération des dimensions réelles
Nt <- dim(data)[1]
Ns <- dim(data)[2]
Nv <- dim(data)[3]

# 0.
test_that("generate_variable_index_pairs returns correct structure", {
  res <- generate_variable_index_pairs(names)
  
  expect_is(res, "data.frame")
  expect_equal(names(res), c("v1", "v2"))
  expect_true(all(res$v1 %in% names))
  expect_true(all(res$v2 %in% names))
  expect_equal(dim(res)[1], Nv*(Nv+1)/2)
  
})

# 1.
test_that("generate_variable_index_pairs includes all self-pairs and unique cross-pairs", {
  res <- generate_variable_index_pairs(names)
  
  # Nombre total attendu : n(n+1)/2
  expected_n <- length(names)*(length(names)+1)/2
  expect_equal(nrow(res), expected_n)
  
  # Self-pairs
  selfpairs <- res[res$v1 == res$v2, ]
  expect_equal(nrow(selfpairs), length(names))
  
  # Unicité : pas de doublons type (X,Y) et (Y,X)
  for (i in seq_along(names)) {
    for (j in seq_along(names)) {
      if (i < j) {
        X <- names[i]
        Y <- names[j]
        
        idx <- with(
          res,
          (v1 == X & v2 == Y) | (v1 == Y & v2 == X)
        )
        
        # no doublon allowed: at most 1 row
        expect_lte(sum(idx), 1)
        
      }
    }
  }
})

# 2.
test_that("generate_variable_index_pairs returns self-pairs first", {
  res <- generate_variable_index_pairs(names)
  
  # Les self-pairs doivent être en haut du datafame
  expect_true(all(res$v1[1:3] == res$v2[1:3]))
})

# 3.
test_that("generate_variable_index_pairs handles a single variable", {
  names <- c("Temp_max")
  res <- generate_variable_index_pairs(names)
  
  expect_equal(nrow(res), 1)
  expect_equal(res$v1, "Temp_max")
  expect_equal(res$v2, "Temp_max")
})

# 4.
test_that("generate_variable_index_pairs handles an empty vector", {
  names <- character(0)
  res <- generate_variable_index_pairs(names)
  
  expect_is(res, "data.frame")
  expect_equal(nrow(res), 0)
  expect_equal(ncol(res), 2)
})


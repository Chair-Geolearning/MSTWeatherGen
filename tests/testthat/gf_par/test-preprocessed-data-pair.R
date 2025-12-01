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

wt_id <- 100:120
tmax <- 4

# Parametres.
Ti <- generate_temporal_index_pairs(wt_id, dates, tmax)
Si <- generate_spatial_index_pairs(coordinates, n1 = 3, n2 = 2)

# 0.
test_that("preprocess_data_mst returns the expected structure", {
  res <- preprocess_data(Ti, Si, coordinates)
  
  expect_true(is.list(res))
  expect_true(all(c("uh", "u", "h") %in% names(res)))
  expect_true(is.matrix(res$uh))
  expect_true(is.numeric(res$u))
  expect_true(is.numeric(res$h))
  
})

# 1.
test_that("Dimensions of u, h, uh are coherent", {
  res <- preprocess_data(Ti, Si, coordinates)
  
  nT <- nrow(Ti)
  nS <- nrow(Si)
  npairs <- nT * nS
  
  expect_equal(length(res$u), npairs)
  expect_equal(length(res$h), npairs)
  expect_equal(nrow(res$uh), npairs)
  expect_equal(ncol(res$uh), 6)
  expect_equal(length(res$u),length(res$h))
  expect_equal(length(res$u),dim(res$uh)[1])
  
})

# 2.
test_that("uh combines components correctly", {
  res <- preprocess_data(Ti, Si, coordinates)
  
  uh <- res$uh
  
  expect_equal(uh[,1], res$u)      # lag
  expect_equal(uh[,2], res$h)      # distance
})

# 3.
test_that("Temporal lags and spatial distances are valid", {
  res <- preprocess_data(Ti, Si, coordinates)
  
  expect_true(all(res$u >= 0))           # lags are non-negative
  expect_true(all(res$h >= 0))           # distances are non-negative
  expect_false(any(is.na(res$u)))        # no NA
  expect_false(any(is.na(res$h)))
})

# 4.
test_that("Indices in uh match Ti and Si", {
  res <- preprocess_data(Ti, Si, coordinates)
  uh <- res$uh
  
  # Vérifie que les indices temporels proviennent bien de Ti
  expect_true(all(uh[,3] %in% Ti[,1]))
  expect_true(all(uh[,4] %in% Ti[,2]))
  
  # Vérifie que les indices spatiaux proviennent bien de Si
  expect_true(all(uh[,5] %in% Si[,1]))
  expect_true(all(uh[,6] %in% Si[,2]))
})

# 5.
test_that("preprocess_data_mst works on minimal inputs", {
  Ti_min <- matrix(c(1, 2, 3), ncol = 3)
  Si_min <- matrix(c(1, 2), ncol = 2)
  coord_min <- matrix(c(0,0, 1,0), ncol=2, byrow=TRUE)
  
  res <- preprocess_data(Ti_min, Si_min, coord_min)
  
  expect_equal(length(res$u), 1)
  expect_equal(length(res$h), 1)
  expect_equal(ncol(res$uh), 6)
})

# 6.
test_that("preprocess_data_mst throws errors on invalid inputs", {
  Ti_bad <- matrix(1:4, ncol = 2)      # pas assez de colonnes
  Si_bad <- matrix(1:3, ncol = 1)      # pas assez de colonnes
  
  expect_error(preprocess_data(Ti_bad, Si, coordinates))
  expect_error(preprocess_data(Ti, Si_bad, coordinates))
  expect_error(preprocess_data(Ti, Si, matrix(1:3, ncol=1)))
})


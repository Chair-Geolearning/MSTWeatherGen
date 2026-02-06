# Libraries:
library(testthat)

# Data:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"),as.Date("2021-12-31"), by="day")
names = c("Precipitation", "Wind", "Temp_max")

# Retrieve results:

# Parameters
set.seed(1)
wt <- resultperm$cluster
K <- length(unique(wt))

# Dimensions 
Nt <- dim(data)[1]
Ns <- dim(data)[2]
Nv <- dim(data)[3]

# --- Tests for generate_temporal_index_pairs ---

# 0. 
test_that("generate_temporal_index_pairs returns a matrix with 3 columns", {
  wt_id <- 10:20
  res <- generate_temporal_index_pairs(wt_id, dates, tmax = 3)
  
  expect_type(res, "integer")
  expect_equal(ncol(res), 3)
  expect_equal(colnames(res), c("t1", "t2", "u"))
})

# 1. 
test_that("generate_temporal_index_pairs returns valid time indices", {
  wt_id <- 15:25
  tmax <- 4
  res <- generate_temporal_index_pairs(wt_id, dates, tmax)
  
  expect_true(all(res[,1] >= 1 & res[,1] <= Nt))
  expect_true(all(res[,2] >= 1 & res[,2] <= Nt))
})

# 2. 
test_that("temporal pairs have correct time differences", {
  wt_id <- 30:40
  tmax <- 5
  res <- generate_temporal_index_pairs(wt_id, dates, tmax)
  
  diffs <- as.numeric(dates[res[,"t2"]] - dates[res[,"t1"]])
  expect_equal(diffs, res[,"u"])
})

# 3. tmax = 0 => uniquement paires avec u = 0
test_that("generate_temporal_index_pairs handles tmax = 0", {
  wt_id <- 50:60
  res <- generate_temporal_index_pairs(wt_id, dates, tmax = 0)
  
  expect_true(all(res[,"u"] == 0))
  expect_equal(res[,"t1"], res[,"t2"])  # Same indices
})

# 4. Bord inférieur : indices trop petits doivent être exclus Jeff
test_that("generate_temporal_index_pairs excludes invalid negative time indices", {
  wt_id <- 1:10  # inclut le bord
  tmax <- 5

  expect_warning(
    expect_error(
      res <- generate_temporal_index_pairs(wt_id, dates, tmax),
    )
  )
  # Aucune ligne ne doit référencer un indice < 1 
  #expect_true(all(res[,"t1"] >= 1))
  #expect_true(all(res[,"t2"] >= 1))
})

# 5. Bord supérieur : rien au-delà de Nt
test_that("generate_temporal_index_pairs excludes time indices beyond Nt Jeff", {
  Nt <- length(dates)
  
  wt_id <- (Nt-5):Nt
  tmax <- 3
  res <- generate_temporal_index_pairs(wt_id, dates, tmax)

  expect_lte(max(res[, "t1"]), Nt)
  expect_lte(max(res[, "t2"]), Nt)
  
})

# 6. Cohérence du nombre potentiel de paires
test_that("generate_temporal_index_pairs number of pairs does not exceed (length(wt_id)*(tmax+1))", {
  wt_id <- 100:120
  tmax <- 4
  res <- generate_temporal_index_pairs(wt_id, dates, tmax)
  
  expect_lte(nrow(res), length(wt_id) * (tmax + 1))
})

# 7. Vérification explicite sur un exemple simple
# test_that("generate_temporal_index_pairs produces correct pairs for a simple case", {
#   simple_dates <- as.Date("2020-01-01") + 0:10
#   wt_id <- 5:7
#   tmax <- 2
  
#   res <- generate_temporal_index_pairs(wt_id, simple_dates, tmax)
  
  # pairs expected manually :
  # t=5 -> (5,5,0), (4,5,1), (3,5,2)
  # t=6 -> (6,6,0), (5,6,1), (4,6,2)
  # t=7 -> (7,7,0), (6,7,1), (5,7,2)
#   expected <- rbind(
#     c(5,5,0), c(4,5,1), c(3,5,2),
#     c(6,6,0), c(5,6,1), c(4,6,2),
#     c(7,7,0), c(6,7,1), c(5,7,2)
# )
  
#   expect_equal(res, expected)
# })

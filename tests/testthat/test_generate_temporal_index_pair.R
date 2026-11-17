# Libraries:
library(testthat)

# Data:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names <- c("Precipitation", "Wind", "Temp_max")
dates <- seq(as.Date("2018-01-01"), as.Date("2021-12-31"), by = "day")
names <- c("Precipitation", "Wind", "Temp_max")

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

'# 0.
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

  expect_true(all(res[, 1] >= 1 & res[, 1] <= Nt))
  expect_true(all(res[, 2] >= 1 & res[, 2] <= Nt))
})

# 2.
test_that("temporal pairs have correct time differences", {
  wt_id <- 30:40
  tmax <- 5
  res <- generate_temporal_index_pairs(wt_id, dates, tmax)

  diffs <- as.numeric(dates[res[, "t2"]] - dates[res[, "t1"]])
  expect_equal(diffs, res[, "u"])
})

# 3. tmax = 0 => uniquement paires avec u = 0
test_that("generate_temporal_index_pairs handles tmax = 0", {
  wt_id <- 50:60
  res <- generate_temporal_index_pairs(wt_id, dates, tmax = 0)

  expect_true(all(res[, "u"] == 0))
  expect_equal(res[, "t1"], res[, "t2"]) # Same indices
})

# 4. Bord inférieur : indices trop petits doivent être exclus Jeff
test_that("generate_temporal_index_pairs excludes invalid negative time indices", {
  wt_id <- 1:10 # inclut le bord
  tmax <- 5

  expect_warning(
    expect_error(
      res <- generate_temporal_index_pairs(wt_id, dates, tmax),
    )
  )
  # Aucune ligne ne doit référencer un indice < 1
  # expect_true(all(res[,"t1"] >= 1))
  # expect_true(all(res[,"t2"] >= 1))
})

# 5. Bord supérieur : rien au-delà de Nt
test_that("generate_temporal_index_pairs excludes time indices beyond Nt Jeff", {
  Nt <- length(dates)

  wt_id <- (Nt - 5):Nt
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
'
# 7. Cohérence du nombre potentiel de paires
test_that("generate_temporal_index_pairs indices stay within data dimensions", {
  wt_id <- which(wt == 1)   # weather type 1, indices réels
  tmax  <- 3
  res   <- generate_temporal_index_pairs(wt_id, dates, tmax)
  
  # Nt = 4018 (dim(data)[1])
  expect_true(all(res[, "t1"] >= 1))
  expect_true(all(res[, "t1"] <= Nt))
  expect_true(all(res[, "t2"] >= 1))
  expect_true(all(res[, "t2"] <= Nt))
})

# 8. t1 appartient toujours au même weather type
test_that("generate_temporal_index_pairs : t1 toujours dans wt_id", {
  wt_id <- which(wt == 1)
  res   <- generate_temporal_index_pairs(wt_id, dates, tmax = 3)
  
  expect_true(all(res[, "t1"] %in% wt_id))
  expect_true(all(res[, "t2"] %in% wt_id))
})

# 9. Pas de NA dans le résultat
test_that("generate_temporal_index_pairs : aucun NA", {
  wt_id <- which(wt == 1)
  res   <- generate_temporal_index_pairs(wt_id, dates, tmax = 3)
  
  expect_false(anyNA(res))
})


# 10. La différence de dates correspond exactement au lag u
test_that("generate_temporal_index_pairs : diff dates == u pour tous les WT", {
  for (k in 1:K) {
    wt_id <- which(wt == k)
    res   <- generate_temporal_index_pairs(wt_id, dates, tmax = 3)
    
    diffs <- as.numeric(dates[res[, "t2"]] - dates[res[, "t1"]])
    expect_equal(diffs, res[, "u"],
                 info = paste("Echec pour weather type", k))
  }
})

# 11. t2 > t1 toujours (sauf lag 0)
test_that("generate_temporal_index_pairs : t2 >= t1", {
  wt_id <- which(wt == 1)
  res   <- generate_temporal_index_pairs(wt_id, dates, tmax = 3)
  
  expect_true(all(res[, "t2"] >= res[, "t1"]))
})

# 12. Les lags présents sont exactement entre 0:tmax
test_that("generate_temporal_index_pairs : lags couvrent 0:tmax", {
  wt_id <- which(wt == 1)
  tmax  <- 3
  res   <- generate_temporal_index_pairs(wt_id, dates, tmax = tmax)
  
  expect_setequal(unique(res[, "u"]), 0:tmax)
})

# 13. Stress test : indices en début de dataset + grand tmax
test_that("generate_temporal_index_pairs : petits indices + grand tmax", {
  # Seulement 2 indices proches du début → wt_id - i <= 0 très vite
  wt_id <- c(2, 3)
  tmax  <- 10   # bien plus grand que max(wt_id)
  res   <- generate_temporal_index_pairs(wt_id, dates, tmax)
  
  # Pas de NA
  expect_false(anyNA(res))
  
  # t1 toujours >= 1 (jamais 0 ou car il y a le timelag nul)
  expect_true(all(res[, "t1"] >= 1))
  
  # t1 dans wt_id
  expect_true(all(res[, "t1"] %in% wt_id))
  
  # Seuls lags possibles : 0 (t1==t2) et 1 (t1=2,t2=3)
  expect_true(all(res[, "u"] %in% c(0, 1)))
  
  # Aucun lag >= 2 possible car max(wt_id) - min(wt_id) = 1
  expect_false(any(res[, "u"] >= 2))
})
# 14. Stress test : wt_id = c(1) seul indice au bord absolu
test_that("generate_temporal_index_pairs : wt_id = c(1) + grand tmax", {
  wt_id <- c(1)
  tmax  <- 5
  res   <- generate_temporal_index_pairs(wt_id, dates, tmax)
  
  # Seul lag=0 possible : t1 = t2 = 1
  expect_false(anyNA(res))
  expect_true(all(res[, "u"] == 0))
  expect_true(all(res[, "t1"] == 1))
  expect_true(all(res[, "t2"] == 1))
})


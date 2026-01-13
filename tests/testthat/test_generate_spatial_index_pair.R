# Libraries:
library(testthat)

# Data to be tested on:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"),as.Date("2021-12-31"), by="day")
names = c("Precipitation", "Wind", "Temp_max")

resultperm <- readRDS(testthat::test_path("saved_results/resultperm2.rds"))

set.seed(1)
wt <- resultperm$cluster
K <- length(unique(wt))

# Parameters
Nt <- dim(data)[1]
Ns <- dim(data)[2]
Nv <- dim(data)[3]

# --- Tests for generate_spatial_index_pairs ---

# 0. Test de structure de sortie
test_that("generate_spatial_index_pairs returns a matrix with 2 columns", {
  res <- generate_spatial_index_pairs(coordinates, n1 = 3, n2 = 2)
  
  expect_type(res, "double")
  expect_equal(ncol(res), 2)
})

# 1. Test de type et valeurs
test_that("generate_spatial_index_pairs returns valid indices within range", {
  res <- generate_spatial_index_pairs(coordinates, n1 = 3, n2 = 2)
  
  expect_true(all(res >= 1 & res <= Ns))
  expect_true(all(res[,1] <= res[,2])) # min-max ordering
})

# 2. Test d'unicité
test_that("generate_spatial_index_pairs returns unique pairs", {
  res <- generate_spatial_index_pairs(coordinates, n1 = 3, n2 = 2)
  
  pairs_strings <- apply(res, 1, function(x) paste(x, collapse = "-"))
  expect_equal(length(pairs_strings), length(unique(pairs_strings)))
})

# 3. Test pour n1 = 0 ou n2 = 0
test_that("generate_spatial_index_pairs handles n1 = 0 or n2 = 0", {
  expect_error(res0 <- generate_spatial_index_pairs(coordinates, n1 = 0, n2 = 2))

  res0b <- generate_spatial_index_pairs(coordinates, n1 = 3, n2 = 0)
  expect_equal(nrow(res0b), 3)
})

# 4. Test pour une seule coordonnée
test_that("generate_spatial_index_pairs works with single point", {
  single_coord <- coordinates[1, , drop = FALSE]
  res <- generate_spatial_index_pairs(single_coord, n1 = 1, n2 = 1)
  
  expect_equal(nrow(res), 1)
  expect_equal(res[1,1], 1)
  expect_equal(res[1,2], 1)
})

# 5. Cohérence globale
test_that("generate_spatial_index_pairs all pairs have min index first", {
  res <- generate_spatial_index_pairs(coordinates, n1 = 4, n2 = 3)
  expect_true(all(res[,1] <= res[,2]))
})

# 6. Test sortie cohérente avec n1 et n2
test_that("generate_spatial_index_pairs output length depends on n1 and n2", {
  res <- generate_spatial_index_pairs(coordinates, n1 = 5, n2 = 2)
  expect_lte(nrow(res), 5*2+5) # au maximum n1*n2 paires plus doublons.
})

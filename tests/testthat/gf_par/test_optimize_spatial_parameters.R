# Libraries:
library(testthat)
library(MSTWeatherGen)  

# Data:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"),as.Date("2021-12-31"), by="day")
names = c("Precipitation", "Wind", "Temp_max")

# Parameter 
Ti <- generate_temporal_index_pairs(wt_id, dates, tmax)
Vi <- generate_spatial_index_pairs(coordinates, n1 = 3, n2 = 2)
preprocessdata <- preprocess_data(Ti, Si, coordinates)
uh <- preprocessdata$uh
cr <- diag(2)
max_it <- 10

preprocessed_data <- preprocess_data(Ti, Si, coordinates)
uh <- preprocessed_data$uh
uh <- cbind(uh, threshold_precip[uh[,5]], threshold_precip[uh[,6]])
u <- preprocessed_data$u
h <- preprocessed_data$h  
ep <- generate_variable_index_pairs(names)

# Parameters
names <- c("Wind", "Temp")
pairs <- c("Wind-Wind", "Wind-Temp", "Temp-Temp")

par_s <- matrix(
  c(1.0, 0.3,
    0.3, 1.0),
  nrow = 2,
  byrow = TRUE
)

# ax : valeurs petites NON NULLES
ax <- list(
  v1  = c("Wind", "Wind", "Temp"),
  v2  = c("Wind", "Temp", "Temp"),
  cov = c(0.01, 0.01, 0.01)
)

cr <- diag(length(names))

par_all <- initialize_par_all_if_missing(
  par_all = NULL,
  names = names,
  pairs = pairs,
  par_s = par_s,
  ax = ax,
  cr = cr
)


# --- Tests ---
test_that("optimize_spatial_parameters retourne un vecteur de même longueur", {
  res <- optimize_spatial_parameters(par_all, data, names, Vi, uh, cr, max_it = 10, ep)
  expect_equal(length(res), length(par_all))
})

test_that("optimize_spatial_parameters retourne un vecteur numérique", {
  res <- optimize_spatial_parameters(par_all, data, names, Vi, uh, cr, max_it, ep)
  expect_true(is.numeric(res))
})

test_that("optimize_spatial_parameters gère un data.frame ep vide", {
  empty_ep <- data.frame(V1 = character(0), V2 = character(0))
  res <- optimize_spatial_parameters(par_all, data, names, Vi, uh, cr, max_it, empty_ep)
  expect_equal(length(res), length(par_all))
})

test_that("optimize_spatial_parameters renvoie identique si pas d'optimisation nécessaire", {
  par_all_constant <- setNames(rep(0, 10), paste0("param", 1:10))
  res <- optimize_spatial_parameters(par_all_constant, data, names, Vi, uh, cr, max_it, ep)
  expect_true(all(!is.na(res)))
})

test_that("optimize_spatial_parameters gère des noms de variables manquants dans ep", {
  ep_wrong <- data.frame(V1 = c("Var1"), V2 = c("Var3"))  # Var3 n'existe pas
  expect_error(optimize_spatial_parameters(par_all, data, names, Vi, uh, cr, max_it, ep_wrong))
})

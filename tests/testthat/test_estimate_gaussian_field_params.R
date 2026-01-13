# Libraries:
library(testthat)

# Data:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"),as.Date("2021-12-31"), by="day")
names = c("Precipitation", "Wind", "Temp_max")

s1 = list(min_day = 1, max_day = 29, min_month = 12, max_month = 2)

filtered = filter_season_data(data, dates, s1, names)
data = filtered$data_filtered
dates = filtered$dates_filtered
rm(filtered)
names_weather_types = names
K <- 5

#Reproducing results
'# Parameters
K <- length(unique(wt))
n1 <- 3
n2 <- 4  
tmax <- 1  
max_it <- 100

seasons <- list(
  s1 = list(min_day = 1, max_day = 29, min_month = 12, max_month = 2),
  s2 = list(min_day = 1, max_day = 31, min_month = 3, max_month = 5),
  s3 = list(min_day = 1, max_day = 31, min_month = 6, max_month = 8),
  s4 = list(min_day = 1, max_day = 30, min_month = 9, max_month = 11)
)


Nt <- dim(data)[1]
Ns <- dim(data)[2]
Nv <- dim(data)[3]

lmbd = estimate_lambda_transformations(data = data, wt = wt, names = names_weather_types, coordinates = coordinates)
threshold_precip = lmbd$threshold_precip


swg <- lapply(seasons["s1"], function(season) {
  # Step 1: Filter the input data and dates for the specified season
  filtered = filter_season_data(data, dates, season, names)
  data = filtered$data_filtered
  dates = filtered$dates_filtered
  rm(filtered)
  
  # Step 3-1: Identify weather types for the season
  wt = weather_types(data = data, variables = names_weather_types, dates = dates,coordinates =  coordinates,
                     max_number_wt = 6, return_plots = F)
  wt = wt$cluster # extract weather types 
  
  # Step 3-2: Estimate transition probabilities between weather types
  transitions = estimate_transitions(cluster = wt, dates = dates, nb = 30, K = length(unique(wt)))
  
  # Step 4: Transformations for each variable in each weather type
  lmbd = estimate_lambda_transformations(data = data, wt = wt, names = names, coordinates = coordinates)
  threshold_precip = lmbd$threshold_precip
  lmbd = lmbd$lambda_transformations
  data = transformations(data = data,wt = wt,names = names, coordinates = coordinates,lmbd = lmbd)
  
  # Step 5: Estimate parameters for the Gaussian field model
  gf_par = estimate_gaussian_field_params(data = data, wt = wt, names = names, coordinates = coordinates, 
                                          tmax = tmax, max_it = max_it, n1 = n1, n2 = n2, 
                                          dates = dates, threshold_precip = threshold_precip)
})

saveRDS(swg, file = "estimate_gaussian_fields_params.rds")'

result_estimate_gaussian_fields_params <- readRDS(testthat::test_path("saved_results/estimate_gaussian_fields_params.rds"))

# --- Tests ---
# 0.
test_that("Structure of results", {
 
  expect_type(result_estimate_gaussian_fields_params, "list")
  expect_length(result_estimate_gaussian_fields_params$s1,5)
 
})

# 1.
test_that("Structure of sub results", {
  
  lapply(1:K, function(k) {
    expect_type(result_estimate_gaussian_fields_params$s1[[k]], "list")
    
  })
})

# 2.
test_that("Dimension of results", {
  
  ref_dim <- dim(result_estimate_gaussian_fields_params$s1[[1]])

  lapply(1:K, function(k) {
    expect_equal(
      dim(result_estimate_gaussian_fields_params$s1[[k]]),
      ref_dim
    )
  })
})

# 3.
test_that("All numerical values are finite and no NA", {
  
  lapply(1:K, function(k) {
    num <- result_estimate_gaussian_fields_params$s1[[k]][sapply(result_estimate_gaussian_fields_params$s1[[k]], is.numeric)]
    
    expect_true(all(!is.na(unlist(num))))
    expect_true(all(is.finite(unlist(num))))
  })
})




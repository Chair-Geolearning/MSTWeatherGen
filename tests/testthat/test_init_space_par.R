# Libraries:
library(testthat)

# Data:

data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"),as.Date("2021-12-31"), by="day")
names = c("Precipitation", "Wind", "Temp_max")

seasons <- list(
  s1 = list(min_day = 1, max_day = 29, min_month = 12, max_month = 2),
  s2 = list(min_day = 1, max_day = 31, min_month = 3, max_month = 5),
  s3 = list(min_day = 1, max_day = 31, min_month = 6, max_month = 8),
  s4 = list(min_day = 1, max_day = 30, min_month = 9, max_month = 11)
)

s1 = list(min_day = 1, max_day = 29, min_month = 12, max_month = 2)
filtered = filter_season_data(data, dates, s1, names)
data = filtered$data_filtered
dates = filtered$dates_filtered
rm(filtered)
names_weather_types = names
K <- 5
n1 <- 3
n2 <- 4  
tmax <- 1  
max_it <- 100

# Generate spatial, temporal, and variable index pairs
wt = weather_types(data = data, variables = names_weather_types, dates = dates,coordinates =  coordinates,
                   max_number_wt = 6, return_plots = F)
wt = wt$cluster # extract weather types 

#saveRDS(wt,'wt_cluster.rds')
wt <- readRDS(testthat::test_path("saved_results/wt_cluster.rds"))

wt_id <- which(wt == 1)
wt_id <- wt_id[wt_id > tmax + 1]  

# Dimensions of the data
Nt <- dim(data)[1] 
Ns <- dim(data)[2]  
Nv <- dim(data)[3]  

# Generate spatial, temporal, and variable index pairs
Si <- generate_spatial_index_pairs(coordinates, n1=n1, n2=n2)
Ti <- generate_temporal_index_pairs(wt_id, dates, tmax)
Vi <- generate_variable_index_pairs(names)

# Preprocess data to adjust for thresholds and compute distances
preprocessed_data <- preprocess_data(Ti, Si, coordinates)
uh <- preprocessed_data$uh
#uh <- cbind(uh, threshold_precip[uh[,5]], threshold_precip[uh[,6]])
u <- preprocessed_data$u
h <- preprocessed_data$h  

# SImple case:
#h <- as.vector(dist(coordinates))
#uh <- matrix(1, nrow = length(h), ncol = length(h))  


#saveRDS(result, file = "pars.rds")

result <- readRDS(testthat::test_path("saved_results/pars.rds"))

# --- Tests ---

# 0.
test_that("init_space_par works and optimisation has worked effectively", {
  expect_equal(length(result), length(names))
  
  for (res in result) {
    expect_true(is.numeric(res))
    expect_equal(length(res), 2)
  }
})

# 1.
test_that("init_space_par is reproducible with fixed seed", {
  skip_on_cran()
  skip_on_ci()
  n_replications <- 50
  seed_value <- 1243
  
  results <- lapply(1:n_replications, function(i) {
    set.seed(seed_value)
    invisible(capture.output({init_space_par(data, names, h, uh, max_it = 50) }))
  })
  
  reference_result <- results[[1]]
  all_equal <- all(sapply(2:n_replications, function(i) {
    identical(results[[1]], results[[i]])
  }))
  
  expect_equal(all_equal,TRUE)
})

# 2.
test_that("init_space_par is reproducible with another seed", {
  skip_on_cran()
  skip_on_ci()
  n_replications <- 50
  seed_value <- 123
  
  resultsbis <- lapply(1:n_replications, function(i) {
    set.seed(seed_value)
    invisible(capture.output({init_space_par(data, names, h, uh, max_it = 50) }))
  })
  
  reference_result <- resultsbis[[1]]
  
  all_equal <- all(sapply(2:n_replications, function(i) {
    identical(resultsbis[[1]], resultsbis[[i]])
  }))
  
  expect_equal(all_equal,TRUE)
})


# 3.
test_that("init_space_par is different when iterations are differents", {
  resultsb1 <- init_space_par(data, names, h, uh, max_it = 50)
  resultsb2 <- init_space_par(data, names, h, uh, max_it = 55)
  
  expect_equal(identical(resultsb1,resultsb2),FALSE)
})
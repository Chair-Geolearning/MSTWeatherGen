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

'filtered = filter_season_data(data, dates, s1, names)
data = filtered$data_filtered
dates = filtered$dates_filtered
rm(filtered)
names_weather_types = names
K <- 5

# Generate spatial, temporal, and variable index pairs
wt = weather_types(data = data, variables = names_weather_types, dates = dates,coordinates =  coordinates,
                   max_number_wt = 6, return_plots = F)
wt = wt$cluster # extract weather types 

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
uh <- cbind(uh, threshold_precip[uh[,5]], threshold_precip[uh[,6]])
u <- preprocessed_data$u
h <- preprocessed_data$h  

# SImple case:
#h <- as.vector(dist(coordinates))
#uh <- matrix(1, nrow = length(h), ncol = length(h))  

result <- init_space_par(data, names, h, uh, max_it = 50)
saveRDS(result, file = "pars.rds")'

result <- par_s
# --- Tests ---

# 0.
test_that("init_space_par works and optimisation has worked effectively", {
  
  expect_equal(length(result), length(names))
  
  for (res in result) {
    expect_true(is.numeric(res))
    expect_equal(length(res), 2)
  }
})

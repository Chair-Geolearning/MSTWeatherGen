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

par_all <-  readRDS(testthat::test_path("saved_results/par_all.rds"))
Vi <-  readRDS(testthat::test_path("saved_results/Vi.rds"))
uh <-  readRDS(testthat::test_path("saved_results/uh.rds"))
uh <- uh[uh[,1]==0,]
cr <- readRDS(testthat::test_path("saved_results/cr.rds"))
ep <- readRDS(testthat::test_path("saved_results/ep.rds"))
max_it <- 50

par_all <- optimize_spatial_parameters(par_all, data, names, Vi, uh[uh[,1]==0,], cr, max_it, ep)

# TO reproduce results
'filtered = filter_season_data(data, dates, s1, names)
data = filtered$data_filtered
dates = filtered$dates_filtered
rm(filtered)
names_weather_types = names

K <- 5
n1 <- 3
n2 <- 4  
tmax <- 1  
max_it <- 50

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


K = length(unique(wt))
# Initialize the Gaussian field parameters storage
gf_par <- vector(mode = "list", length = K)

# Compute average pairwise correlations for each pair of variables across all locations
ep <- generate_variable_index_pairs(names)
# Estimate spatial covariance structures for each pair of variables 
dst = sapply(1:nrow(coordinates), function(i){
  sapply(1:nrow(coordinates), function(j){
    geosphere::distHaversine(coordinates[i,], coordinates[j,])/1000
  })
})

vgm = lapply(1:nrow(ep), function(i){
  variable = unlist(ep[i,])
  dist = sort(unique(c(floor(dst))))
  dist = dist[seq(1, length(dist)/1.5, length.out = 2)]
  vgm = spacetime_cov(data = data[,,variable],wt_id = 2:dim(data)[1], locations = coordinates, ds = dst,
                      dates = dates, lagstime = 0, dist = dist, covgm = T)
  vgm$v = paste(variable[1], variable[2], sep = "-")
  vgm$v1 = variable[1]
  vgm$v2 = variable[2]
  
  return(vgm)
})

vgm = do.call(rbind, vgm)
cr <- sapply(names, function(v1) {
  sapply(names, function(v2) {
    mean(sapply(1:dim(data)[2], function(j) cor(data[, j, v1], data[, j, v2], use = "complete.obs")), na.rm = TRUE)
  })
})

colnames(cr) <- rownames(cr) <- names
# For each weather type, estimate Gaussian field parameters

k <- 3

ax <-  vgm[vgm$lagtime==0&vgm$dist==max(vgm$dist),]
  
threshold_precip <- threshold_precip[[k]]
  
wt_id <- which(wt == k)
wt_id <- wt_id[wt_id > tmax + 1]  
  
#Estimate Gaussian field parameters
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
  
  
# Initialize spatial parameters 
par_s <- init_space_par(data = data, names = names, h = h[u == 0], uh = uh[u == 0,], max_it = max_it)
par_s <- do.call(cbind, par_s)
  
# Construct parameter matrix for covariance model
ep <- generate_variable_index_pairs(names)
pairs <- paste(ep[,1],ep[,2], sep = "-")'
  
# Check and initialize par_all if missing
#par_all <- initialize_par_all_if_missing(par_all, names, pairs, par_s, ax, cr = cr)
#saveRDS(par_all,'par_all.rds')
#saveRDS(Vi,'Vi.rds')
#saveRDS(uh,'uh.rds')
#saveRDS(cr,'cr.rds')
#saveRDS(ep,'ep.rds')


'for (v in 1:2) {
    # Optimize temporal parameters
    par_all <- optimize_temporal_parameters(par_all, data, names, Vi, uh, cr, max_it, ep)
    # Optimize spatial parameters
    par_all <- optimize_spatial_parameters(par_all, data, names, Vi, uh, cr, max_it, ep)
}
'

# 0.
test_that("optimize_temporal_parameters is reproducible with fixed seed", {
  n_replications <- 50
  seed_value <- 1243
  
  results <- lapply(1:n_replications, function(i) {
    set.seed(seed_value)
    optimize_temporal_parameters(par_all, data, names, Vi, uh[uh[,1]==0,], cr, max_it, ep)
  })
  
  reference_result <- results[[1]]

  all_equal <- all(sapply(2:n_replications, function(i) {
    identical(results[[1]], results[[i]])
  }))
  
  expect_equal(all_equal,TRUE)
})

# 1.
test_that("optimize_temporal_parameters is reproducible with another seed", {
  n_replications <- 50
  seed_value <- 123
  
  results <- lapply(1:n_replications, function(i) {
    set.seed(seed_value)
    optimize_temporal_parameters(par_all, data, names, Vi, uh[uh[,1]==0,], cr, max_it, ep)
  })
  
  reference_result <- results[[1]]
  
  all_equal <- all(sapply(2:n_replications, function(i) {
    identical(results[[1]], results[[i]])
  }))
  
  expect_equal(all_equal,TRUE)
})





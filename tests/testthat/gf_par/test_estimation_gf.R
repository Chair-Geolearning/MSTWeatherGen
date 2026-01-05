# Libraries:
library(testthat)
library(MSTWeatherGen)  

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

# Parameters: 
'set.seed(1)
resultperm <- readRDS("/home/aboualam/MSTWeatherGen/tests/testthat/resultperm2.rds")

wt <- resultperm$cluster
K <- length(unique(wt))
threshold_precip <- c(0, 1, 2)
n1 <- 2
n2 <- 2
max_it <- 5
tmax <- 2
ax <- list(
  v1  = c("Wind", "Wind", "Temp"),
  v2  = c("Wind", "Temp", "Temp"),
  cov = c(0.01, 0.01, 0.01)
)
cr <- diag(length(names))
vgm <-spacetime_cov(
  data = data[,,variable],
  wt_id = 50:200,
  locations = coordinates,
  ds = dst,
  dates = dates,
  lagstime = 0,
  dist = dist,
  covgm = TRUE
)

ax <- vgm[vgm$lagtime==0&vgm$dist==max(vgm$dist),]'

# Generating a full result on one season and one 
'result_test <- lapply(seasons["s1"], function(season) {
  
  n1 <- 3
  n2 <- 4  
  tmax <- 1  
  max_it <- 100
  
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
  
  K = length(unique(wt))
  
  # Initialize the Gaussian field parameters storage
  gf_par <- vector(mode = "list", length = K)
  
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
 
  wt_id <- which(wt == k)
  wt_id <- wt_id[wt_id > tmax + 1]  
    
  #Estimate Gaussian field parameters
  result_test <- estimation_gf(data = data, wt_id = wt_id, max_it = max_it, dates = dates, 
                                 tmax = tmax, names = names, coordinates = coordinates, n1 = n1, 
                                 n2 = n2, ax = vgm[vgm$lagtime==0&vgm$dist==max(vgm$dist),], 
                                 cr = cr, threshold_precip = threshold_precip[[k]])
  
})

saveRDS(result_test, file = "estimation_gf_results_v1.rds")'

result_test <- readRDS("/home/aboualam/MSTWeatherGen/estimation_gf_results_v1.rds")

# --- Tests ---

# 0.
test_that("Structure of results", {

  expect_type(result_test, "list")
  expect_named(result_test, c("s1"))
  expect_length(result_test, 1)
  
})

# 1.
test_that("All parameters are right in the results", {
  
  expect_type(result_test$s1, "list")
  expect_length(result_test$s1, 2)
  expect_named(result_test$s1, c("parm","par_all"))
  
})

# 2.
test_that("Dimension of results", {
  
  expect_type(result_test$s1, "list")
  expect_length(result_test$s1, 2)
  expect_named(result_test$s1, c("parm","par_all"))
  
})

# 3.
test_that("parm is a list with correct dimensions", {

  expect_type(result_test$s1$parm,'list')
  expect_equal(length(dim(result_test$s1$parm)),2)
})

# 4.
test_that("par_all structure", {
  
  expect_type(result_test$s1$par_all,'double')
  expect_true(length(result_test$s1$par_all) > 0)
})

# 5.
test_that("All numerical values are finite and no NA", {
  
    expect_true(all(!is.na(unlist(result_test$s1$par_all))))
    expect_true(all(is.finite(unlist(result_test$s1$par_all))))
    
    num <- result_test$s1$parm[sapply(result_test$s1$parm, is.numeric)]
    
    expect_true(all(!is.na(unlist(num))))
    expect_true(all(is.finite(unlist(num))))

})

# Add reproducibility test with seed ??


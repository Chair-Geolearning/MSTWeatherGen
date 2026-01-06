# Libraries:
library(testthat)

# Data:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"),as.Date("2021-12-31"), by="day")
names = c("Precipitation", "Wind", "Temp_max")
names_bis = c("Wind", "Temp_max")

# Retrieve results
resultperm <- readRDS("/home/aboualam/MSTWeatherGen/tests/testthat/resultperm2.rds")
wt <- resultperm$cluster
K <- length(unique(wt))

# Dimensions
Nt <- dim(data)[1]
Ns <- dim(data)[2]
Nv <- dim(data)[3]

wt_id <- 2:dim(data)[1]
lagstime <- 0

ep <- generate_variable_index_pairs(names)
variable = unlist(ep[,1])

dst = sapply(1:nrow(coordinates), function(i){
  sapply(1:nrow(coordinates), function(j){
    geosphere::distHaversine(coordinates[i,], coordinates[j,])/1000
  })
})

dist = sort(unique(c(floor(dst))))
dist = dist[seq(1, length(dist)/1.5, length.out = 2)]

'vgm = lapply(1:nrow(ep), function(i){
  variable = unlist(ep[i,])
  dist = sort(unique(c(floor(dst))))
  dist = dist[seq(1, length(dist)/1.5, length.out = 2)]
  
  vgm = spacetime_cov(data = data[,,variable],wt_id = 2:dim(data)[1], locations = coordinates, ds = dst,
                      dates = dates, lagstime = 1, dist = dist, covgm = T)
  return(vgm)
})'

#cov_df <- spacetime_cov(data = data[,,variable], wt_id = 2:dim(data)[1], location = coordinates, ds = dst, dates = dates,
#                        lagstime = 0, dist = dist, covgm = T)
  
# --- Tests ---

# 1.
test_that("spacetime_cov returns a dataframe", {
  cov_df <- spacetime_cov(
    data = data[,,variable],
    wt_id = 50:200,
    locations = coordinates,
    ds = dst,
    dates = dates,
    lagstime = 0,
    dist = dist,
    covgm = TRUE
  )
  
  expect_s3_class(cov_df, "data.frame")
  expect_true(all(c("lagtime", "dist", "cov") %in% colnames(cov_df)))
})

# 2.
test_that("Values are comprised between -1 et 1", {
  cov_df <- cov_df <- spacetime_cov(
    data = data[,,variable],
    wt_id = 50:200,
    locations = coordinates,
    ds = dst,
    dates = dates,
    lagstime = 0,
    dist = dist,
    covgm = TRUE
  )
  
  expect_true(is.numeric(cov_df$cov))
  expect_true(all(cov_df$cov <= 1 & cov_df$cov >= -1))
})

# 3.
test_that("Works with covgm = FALSE", {
  cov_df <- cov_df <- spacetime_cov(
    data = data[,,variable],
    wt_id = 50:200,
    locations = coordinates,
    ds = dst,
    dates = dates,
    lagstime = 0,
    dist = dist,
    covgm = FALSE
  )
  
  expect_s3_class(cov_df, "data.frame")
  expect_true(all(c("lagtime", "dist", "cov") %in% colnames(cov_df)))
})

# 5.
test_that("Returns an error if data is not 3D with covgm = TRUE", {
  expect_error(spacetime_cov(matrix(1:10, 5, 2), wt_id, coordinates,
                             dates = dates, lagstime = lagstime,
                             dist = distances, covgm = TRUE))
})

# 6.
test_that("dist = 0 test", {
  cov_df <- cov_df <- spacetime_cov(
    data = data[,,variable],
    wt_id = 50:200,
    locations = coordinates,
    ds = dst,
    dates = dates,
    lagstime = 0,
    dist = 0,
    covgm = FALSE
  )
  
  expect_true(all(cov_df$dist == 0))
  expect_true(!any(is.na(cov_df$cov)))
})

# 7.
test_that("fonction gère les lagstime correctement", {
  cov_df <- spacetime_cov(
    data = data[,,variable],
    wt_id = 50:200,
    locations = coordinates,
    ds = dst,
    dates = dates,
    lagstime = 0:5,
    dist = dist,
    covgm = FALSE
  )
  
  expect_true(all(unique(cov_df$lagtime) %in% 0:5))
})

# 8.
test_that("Different covariances for different lagstime", {
  cov_df <- spacetime_cov(
    data = data[,,variable],
    wt_id = 50:200,
    locations = coordinates,
    ds = dst,
    dates = dates,
    lagstime = 0:5,
    dist = dist,
    covgm = FALSE
  )
  cov_lag0 <- cov_df$cov[cov_df$lagtime == 0]
  cov_lag1 <- cov_df$cov[cov_df$lagtime == 1]
  
  expect_false(all(cov_lag0 == cov_lag1))
})

# 9.
test_that("Different covariances for different distances", {
  cov_df <- spacetime_cov(
    data = data[,,variable],
    wt_id = 50:200,
    locations = coordinates,
    ds = dst,
    dates = dates,
    lagstime = 0:5,
    dist = dist,
    covgm = FALSE
  )
  
  cov_dist0 <- cov_df$cov[cov_df$dist == 0]
  cov_dist50 <- cov_df$cov[cov_df$dist == 79]
  
  expect_false(all(cov_dist0 == cov_dist50))
})

# 10.
#test_that("This generates NAN, to check again.", {
#  cov_df <  spacetime_cov(data = data[,,variable],wt_id = 2:dim(data)[1], locations = coordinates, ds = dst,
#                               dates = dates, lagstime = 1, dist = dist, covgm = T)
  
#  expect_false(all(cov_dist0 == cov_dist50))
#})


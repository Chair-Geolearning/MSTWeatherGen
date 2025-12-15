# Libraries:
library(testthat)
library(MSTWeatherGen)  

# Data:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"),as.Date("2021-12-31"), by="day")
names = c("Precipitation", "Wind", "Temp_max")
names_bis = c("Wind", "Temp_max")

# Retrieve results
resultperm <- readRDS("resultperm2.rds")
wt <- resultperm$cluster
K <- length(unique(wt))

# Dimensions
Nt <- dim(data)[1]
Ns <- dim(data)[2]
Nv <- dim(data)[3]

# Parameters
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

ax <- vgm[vgm$lagtime==0&vgm$dist==max(vgm$dist),]

cr <- sapply(names, function(v1) {
  sapply(names, function(v2) {
    mean(sapply(1:dim(data)[2], function(j) cor(data[, j, v1], data[, j, v2], use = "complete.obs")), na.rm = TRUE)
  })
})


test_that("initialize_par_all_if_missing initializes par_all", {
  par_all <- initialize_par_all_if_missing(
    par_all = NULL,
    names = c("Wind", "Temp"),
    pairs = c("Wind-Wind", "Wind-Temp", "Temp-Temp"),
    par_s = matrix(c(1, 1, 1, 1), nrow = 2),
    ax = ax,
    cr = diag(2)
  )
  
  expect_type(par_all, "double")
  expect_false(anyNA(par_all))
})

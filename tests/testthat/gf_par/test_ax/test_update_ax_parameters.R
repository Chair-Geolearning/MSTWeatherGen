# Libraries:
library(testthat)
library(MSTWeatherGen)  

# Data:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"),as.Date("2021-12-31"), by="day")
names = c("Precipitation", "Wind", "Temp_max")
resultperm <- readRDS("resultperm2.rds")

# Construct parameter matrix for covariance model
ep <- generate_variable_index_pairs(names)
pairs <- paste(ep[,1],ep[,2], sep = "-")

# Variables
# Dimensions of the data
Nt <- dim(data)[1]  # Number of time points
Ns <- dim(data)[2]  # Number of spatial locations
Nv <- dim(data)[3]  # Number of variables

wt <- resultperm$cluster
wt_id <- which(wt == 3)
wt_id <- wt_id[wt_id >1] 

# Generate spatial, temporal, and variable index pairs
Si <- generate_spatial_index_pairs(coordinates, n1=3, n2=5)
Ti <- generate_temporal_index_pairs(wt_id, dates, tmax = 10)
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



names_par_all <- c(paste(pairs, "dij", sep = ":"), "a1", "d1", "g1", "a2", "d2", "g2",
                   "b1", "e1", "l1", "b2", "e2", "l2", "c", "f", "m",
                   paste(names, "ai", sep = ":"), paste(names, "bi", sep = ":"),
                   paste(names, "ci", sep = ":"),
                   paste(pairs, "rij", sep = ":"), paste(pairs, "vij", sep = ":"), 
                   paste(pairs, "ax", sep = ":"))

par_all <- setNames(rep(0.1, length(names_par_all)), names_par_all)

par_all[paste(pairs, "dij", sep = ":")] = 1
par_all[paste(pairs[1:length(names)], "rij", sep = ":")] <- par_s[1,] 
par_all[paste(pairs[1:length(names)], "vij", sep = ":")] <- par_s[2,] 
par_all[paste(pairs, "ax", sep = ":")] <- 0
parms <- c("a1", "a2", "d1", "d2", "g1", "g2")
par_all[parms] <-rep(1, length(parms))
par_all <- optimize_spatial_parameters(par_all, data, names, Vi, uh[uh[,1]==0,], cr, max_it, ep)

for (v in 1:2) {
  # Optimize temporal parameters
  par_all <- optimize_temporal_parameters(par_all, data, names, Vi, uh, cr, max_it, ep)
  # Optimize spatial parameters
  par_all <- optimize_spatial_parameters(par_all, data, names, Vi, uh, cr, max_it, ep)
}
ax <- compute_ax(param(par_all, names), names)











# ============================================================================
# ============================================================================
test_that("update_ax_parameters works with a valid symmetric PD matrix", {
  res <- update_ax_parameters(par_all, names, ax)
  
  for (v1 in names) {
    for (v2 in names) {
      key <- paste0(v1, "-", v2, ":ax")
      expect_equal(res[key], ax[v1, v2])
    }
  }
})

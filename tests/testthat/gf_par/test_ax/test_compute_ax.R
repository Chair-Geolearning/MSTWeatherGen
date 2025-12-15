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

wt <- resultperm$cluster
K <- length(unique(wt))

# Récupération des dimensions réelles
Nt <- dim(data)[1]
Ns <- dim(data)[2]
Nv <- dim(data)[3]

wt_id <- 100:120
tmax <- 4

# Construct parameter matrix for covariance model
ep <- generate_variable_index_pairs(names)
pairs <- paste(ep[,1],ep[,2], sep = "-")
par_all <- initialize_par_all_if_missing(par_all, names, pairs, par_s, ax, cr = cr)
par_all <- optimize_spatial_parameters(par_all, data, names, Vi, uh[uh[,1]==0,], cr, max_it, ep)

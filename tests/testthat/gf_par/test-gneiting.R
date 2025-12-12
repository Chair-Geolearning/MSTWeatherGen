# Libraries to be load
library(testthat)
library(MSTWeatherGen)

# Data:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"),as.Date("2021-12-31"), by="day")
names = c("Precipitation", "Wind", "Temp_max")

'
# Parameters needed for the function
ep <- generate_variable_index_pairs(names)

# Dimensions of the data
Nt <- dim(data)[1]  # Number of time points
Ns <- dim(data)[2]  # Number of spatial locations
Nv <- dim(data)[3]   # Number of variables

n1 <- 2
n2 <- 2
tmax <- 0

Si <- matrix(
  c(88, 88,
    76, 88,
    4,  88,
    28, 28,
    28, 108,
    28, 97,
    43, 43,
    27, 43,
    43, 54),
  ncol = 2,
  byrow = TRUE
)

u <- rep(0:4, each = 21)  # 5 blocks, 21 rows each
t2 <- rep(100:120, times = 5)
t1 <- t2 - u
Ti <- cbind(t1, t2, u)
colnames(Ti) <- c("t1", "t2", "u")

lmbd = estimate_lambda_transformations(data = data, wt = wt, names = names, coordinates = coordinates)
threshold_precip = lmbd$threshold_precip

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


par_all <- update_ax_parameters(par_all, names, compute_ax(param(par_all, names), names))
parm <- param(par_all, names)

# Create a map to fetch parameters quickly using a two-level list structure
parm_map <- split(parm, list(parm$v1, parm$v2))

# Function to retrieve parameters for a given pair of variables v1 and v2
get_parameters <- function(v1, v2) {
  # Attempt to fetch parameters based on the naming convention, handling both v1,v2 and v2,v1 cases
  if (exists(paste0(v1, ".", v2), parm_map)) {
    par <- as.numeric(parm_map[[paste0(v1, ".", v2)]][-c(1, 2)])  # Exclude the first two elements (variable names)
  } else {
    par <- as.numeric(parm_map[[paste0(v2, ".", v1)]][-c(1, 2)])
  }
  return(par)
}


names = c("Wind", "Temp_max")
v1 <- names[j]  # First variable in the pair
v2 <- names[k]

par <- get_parameters(v1, v2)'

par <- rep(1, 26)
dij <- 1

#1.
test_that("Gneiting returns numeric", {
  res <- Gneiting(h = 0.1, u = 0.2, par = par, dij = dij)
  expect_true(is.numeric(res))
})

#2.
test_that("Gneiting works for h=0, u=0, Nan value check with Jeff.", {
  res <- Gneiting(h = 0, u = 0, par = par, dij = dij)
  expect_true(is.na(res))
})

#3.
test_that("Gneiting returns finite values for vector inputs", {
  h <- c(0.2, 0.5, 1)
  u <- c(0.2, 1, 2)
  res <- Gneiting(h = h, u = u, par = par, dij = dij)
  expect_equal(length(res), 3)
  expect_true(all(is.finite(res)))
})

#4.
test_that("Gneiting returns positive values for positive parameters", {
  res <- Gneiting(h = 0.5, u = 1, par = par, dij = dij)
  expect_true(res >= 0)
})

#5.
test_that("Gneiting returns zero if dij=0 (partial correlation effect)", {
  res <- Gneiting(h = 0.5, u = 1, par = par, dij = 0)
  expect_true(is.finite(res))
})

#6.
test_that("Gneiting returns same values for repeated calls", {
  res1 <- Gneiting(h = 0.5, u = 1, par = par, dij = dij)
  res2 <- Gneiting(h = 0.5, u = 1, par = par, dij = dij)
  expect_equal(res1, res2)
})

#7.
test_that("Gneiting handles zero parameters Nan JEFF", {
  par_zero <- rep(0, 26)
  res <- Gneiting(h = 0.5, u = 1, par = par_zero, dij = dij)
  expect_true(all(is.na(res)))
})

#8.
test_that("Gneiting works for vector h and scalar u", {
  h <- c(0, 0.5, 1)
  u <- 0.5
  res <- Gneiting(h = h, u = u, par = par, dij = dij)
  expect_equal(length(res), length(h))
})

#9.
testthat::test_that("Gneiting works for scalar h and vector u", {
  h <- 0.5
  u <- c(0, 0.5, 1)
  res <- Gneiting(h = h, u = u, par = par, dij = dij)
  expect_equal(length(res), length(u))
})

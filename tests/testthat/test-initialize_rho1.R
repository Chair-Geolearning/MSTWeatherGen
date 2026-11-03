# Libraries:
library(testthat)

# Data:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names <- c("Precipitation", "Wind", "Temp_max")
dates <- seq(as.Date("2018-01-01"), as.Date("2021-12-31"), by = "day")

wt_id <- 2:100

# Build a reference cov_spatial data.frame through process_spatial_covariance
cov_spatial <- process_spatial_covariance(data, dates, coordinates, wt_id, names)

# --- Tests ---

# 1.
test_that("initialize_rho1 returns a symmetric matrix named by variables", {
  rho1 <- initialize_rho1(cov_spatial, names)
  expect_true(is.matrix(rho1))
  expect_equal(dim(rho1), c(3, 3))
  expect_equal(rownames(rho1), names)
  expect_equal(colnames(rho1), names)
  # symmetric
  expect_equal(rho1, t(rho1), tolerance = 1e-12)
})

# 2.
test_that("initialize_rho1 keeps only the largest-distance covariance", {
  cov_max <- cov_spatial[cov_spatial$lagtime == 0 &
                           cov_spatial$dist == max(cov_spatial$dist), ]
  rho1 <- initialize_rho1(cov_spatial, names)

  # w1 as computed in the function
  w1 <- (sqrt((.init[.r1ii]^2 + .init[.r1jj]^2) / 2)) /
    sqrt(.init[.r1ii] * .init[.r1jj])

  for (v in names) {
    expect_equal(
      rho1[v, v],
      cov_max$cov[cov_max$v1 == v & cov_max$v2 == v] * w1,
      tolerance = 1e-12
    )
  }
  # one cross-pair check
  expect_equal(
    rho1["Precipitation", "Wind"],
    cov_max$cov[cov_max$v1 == "Precipitation" & cov_max$v2 == "Wind"] * w1,
    tolerance = 1e-12
  )
})

# 3.
test_that("initialize_rho1 fills the lower triangle symmetrically", {
  rho1 <- initialize_rho1(cov_spatial, names)
  expect_equal(rho1["Wind", "Precipitation"], rho1["Precipitation", "Wind"],
               tolerance = 1e-12)
})

# 4.
test_that("initialize_rho1 is invariant to row reordering of cov_spatial", {
  rho1_orig <- initialize_rho1(cov_spatial, names)
  cov_shuffled <- cov_spatial[sample(nrow(cov_spatial)), ]
  rho1_shuf <- initialize_rho1(cov_shuffled, names)
  expect_equal(rho1_orig, rho1_shuf, tolerance = 1e-12)
})

# 5.
test_that("initialize_rho1 works with a hand-built cov_spatial (3x3)", {
  hand <- data.frame(
    lagtime = rep(0, 6),
    dist    = rep(982, 6),
    cov     = c(1.0, 0.5, 0.3, 0.4, -0.1, 0.2), # v1-v1,v2-v2,v3-v3,v1-v2,v1-v3,v2-v3
    v1 = c("v1", "v2", "v3", "v1", "v1", "v2"),
    v2 = c("v1", "v2", "v3", "v2", "v3", "v3")
  )
  rho1 <- initialize_rho1(hand, c("v1", "v2", "v3"))
  w1 <- (sqrt((.init[.r1ii]^2 + .init[.r1jj]^2) / 2)) /
    sqrt(.init[.r1ii] * .init[.r1jj])

  expect_equal(rho1["v1", "v2"], 0.4 * w1, tolerance = 1e-12)
  expect_equal(rho1["v2", "v1"], 0.4 * w1, tolerance = 1e-12)
  expect_equal(rho1["v1", "v1"], 1.0 * w1, tolerance = 1e-12)
})

# 6.
test_that("initialize_rho1 is robust to extra rows at other lags/distances", {
  # add a spurious row with lagtime = 1 and a smaller distance
  extra <- data.frame(
    lagtime = 1, dist = 0, cov = 99,
    v = "Precipitation-Precipitation",
    v1 = "Precipitation", v2 = "Precipitation"
  )
  rho1 <- initialize_rho1(rbind(cov_spatial, extra), names)
  rho1_ref <- initialize_rho1(cov_spatial, names)
  expect_equal(rho1, rho1_ref, tolerance = 1e-12)
})

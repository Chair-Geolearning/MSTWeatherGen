# Libraries:
library(testthat)

# Data:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names <- c("Precipitation", "Wind", "Temp_max")

# The function expects the full 3D array (data[, , v]) and wt_id as absolute
# time indices. Two variables are needed: data[, , 1:2] -> (Zi, Zj).
wt_id <- 2:100

# Distance matrix (km), matching process_spatial_covariance
dst <- sapply(1:nrow(coordinates), function(i) {
  sapply(1:nrow(coordinates), function(j) {
    geosphere::distHaversine(coordinates[i, ], coordinates[j, ]) / 1000
  })
})
dist <- sort(unique(c(floor(dst))))
start_at_dist <- dist[round(length(dist) / 1.5)]

# --- Tests ---

# 1.
test_that("spatial_tail_covariance returns a data.frame with expected columns", {
  cov_df <- spatial_tail_covariance(
    data = data[, , 1:2], wt_id = wt_id,
    dist_matrix = dst, dist_threshold = start_at_dist
  )
  expect_s3_class(cov_df, "data.frame")
  expect_true(all(c("lagtime", "dist", "cov") %in% colnames(cov_df)))
  expect_equal(nrow(cov_df), 1)
  expect_equal(cov_df$lagtime, 0)
  expect_equal(cov_df$dist, start_at_dist)
  expect_true(is.finite(cov_df$cov))
})

# 2.
test_that("spatial_tail_covariance matches a manual reconstruction", {
  # manual reconstruction using the same pair selection as the function
  x1 <- data[, , 1]  # (Zi)
  x2 <- data[, , 2]  # (Zj)

  # full matrix (both orientations), same as the current implementation
  idx <- which(dst >= start_at_dist, arr.ind = TRUE)
  id_dates <- cbind(wt_id, wt_id)
  d_x_c <- expand.grid(1:nrow(id_dates), 1:nrow(idx))
  id_d <- id_dates[d_x_c[, 1], ]
  id_x <- idx[d_x_c[, 2], ]

  cov_manual <- cov(x1[cbind(id_d[, 1], id_x[, 1])],
                    x2[cbind(id_d[, 2], id_x[, 2])])

  cov_fn <- spatial_tail_covariance(
    data = data[, , 1:2], wt_id = wt_id,
    dist_matrix = dst, dist_threshold = start_at_dist
  )$cov

  expect_equal(cov_fn, cov_manual, tolerance = 1e-12)
})

# 3.
test_that("spatial_tail_covariance is symmetric in the variable pair", {
  # with the full distance matrix the cross-covariance is orientation-averaged,
  # so cov(Zi, Zj) must equal cov(Zj, Zi)
  cov_12 <- spatial_tail_covariance(
    data = data[, , 1:2], wt_id = wt_id,
    dist_matrix = dst, dist_threshold = start_at_dist
  )$cov
  cov_21 <- spatial_tail_covariance(
    data = data[, , 2:1], wt_id = wt_id,
    dist_matrix = dst, dist_threshold = start_at_dist
  )$cov
  expect_equal(cov_12, cov_21, tolerance = 1e-10)
})

# 4.
test_that("threshold 0 includes all site pairs and gives a finite cov", {
  cov_all <- spatial_tail_covariance(
    data = data[, , 1:2], wt_id = wt_id,
    dist_matrix = dst, dist_threshold = 0
  )$cov
  expect_true(is.finite(cov_all))
  n_pairs <- nrow(which(dst >= 0, arr.ind = TRUE))
  expect_gt(n_pairs, 0)
})

# 5.
test_that("spatial_tail_covariance errors when data is not 3D", {
  expect_error(
    spatial_tail_covariance(
      data = matrix(1:10, 5, 2), wt_id = wt_id,
      dist_matrix = dst, dist_threshold = start_at_dist
    )
  )
})

# 6.
test_that("spatial_tail_covariance records the threshold in the dist column", {
  cov_df <- spatial_tail_covariance(
    data = data[, , 1:2], wt_id = wt_id,
    dist_matrix = dst, dist_threshold = start_at_dist
  )
  expect_equal(cov_df$dist, start_at_dist)
})

# Libraries:
library(testthat)

# Data:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
dates <- seq(as.Date("2018-01-01"), as.Date("2021-12-31"), by = "day")
names <- c("Precipitation", "Wind", "Temp_max")

wt_id <- 2:100

# Reference variable pairs
ep <- generate_variable_index_pairs(names)

# --- Tests ---

# 1.
test_that("process_spatial_covariance returns one row per variable pair", {
  cov_df <- process_spatial_covariance(data, dates, coordinates, wt_id, names)
  expect_s3_class(cov_df, "data.frame")
  expect_equal(nrow(cov_df), nrow(ep))
  expect_true(all(c("lagtime", "dist", "cov", "v", "v1", "v2") %in% colnames(cov_df)))
})

# 2.
test_that("all rows are at lag 0 and at the 2/3-distance threshold", {
  cov_df <- process_spatial_covariance(data, dates, coordinates, wt_id, names)
  expect_true(all(cov_df$lagtime == 0))
  # all 'dist' values identical (same threshold for every pair)
  expect_equal(length(unique(cov_df$dist)), 1)
})

# 3.
test_that("each variable pair is covered exactly once", {
  cov_df <- process_spatial_covariance(data, dates, coordinates, wt_id, names)
  key <- paste(cov_df$v1, cov_df$v2, sep = "-")
  ref_key <- paste(ep$v1, ep$v2, sep = "-")
  expect_setequal(key, ref_key)
  expect_equal(length(unique(key)), nrow(cov_df))
})

# 4.
test_that("cov values are finite", {
  cov_df <- process_spatial_covariance(data, dates, coordinates, wt_id, names)
  expect_true(all(is.finite(cov_df$cov)))
})

# 5.
test_that("the v column matches v1-v2", {
  cov_df <- process_spatial_covariance(data, dates, coordinates, wt_id, names)
  expect_equal(cov_df$v, paste(cov_df$v1, cov_df$v2, sep = "-"))
})

# 6.
test_that("process_spatial_covariance agrees with direct spatial_tail_covariance calls", {
  cov_df <- process_spatial_covariance(data, dates, coordinates, wt_id, names)

  # recompute threshold as the function does
  dst <- sapply(1:nrow(coordinates), function(i) {
    sapply(1:nrow(coordinates), function(j) {
      geosphere::distHaversine(coordinates[i, ], coordinates[j, ]) / 1000
    })
  })
  dist <- sort(unique(c(floor(dst))))
  start_at_dist <- dist[round(length(dist) / 1.5)]

  for (i in seq_len(nrow(ep))) {
    variable <- unlist(ep[i, ])
    cov_direct <- spatial_tail_covariance(
      data = data[, , variable], wt_id = wt_id,
      dist_matrix = dst, dist_threshold = start_at_dist
    )$cov
    expect_equal(cov_df$cov[i], cov_direct, tolerance = 1e-12)
  }
})

# 7.
test_that("covariance on the diagonal pairs (self pairs) is a variance", {
  cov_df <- process_spatial_covariance(data, dates, coordinates, wt_id, names)
  # self-pairs (v1 == v2) are variances of a single variable -> >= 0
  self <- cov_df[cov_df$v1 == cov_df$v2, ]
  expect_true(all(self$cov >= 0))
})

# 8.
test_that("process_spatial_covariance works with a small subset of variables", {
  cov_df <- process_spatial_covariance(data, dates, coordinates, wt_id,
                                       c("Precipitation", "Wind"))
  ep2 <- generate_variable_index_pairs(c("Precipitation", "Wind"))
  expect_equal(nrow(cov_df), nrow(ep2))
  expect_setequal(paste(cov_df$v1, cov_df$v2, sep = "-"),
                  paste(ep2$v1, ep2$v2, sep = "-"))
})

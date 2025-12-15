library(testthat)
library(MSTWeatherGen)


#1.
test_that("Matern returns 1 when h = 0 because the Bessel value at 0 is not defined", {
  expect_equal(Matern(0, r = 1, v = 0.5), 1)
  expect_equal(Matern(c(0, 0, 1), r = 1, v = 1), c(1, 1, Matern(1, 1, 1)))
})

#2.
test_that("Matern handles vector inputs correctly", {
  h <- c(0, 0.5, 1, 2)
  res <- Matern(h, r = 1, v = 0.5)
  expect_equal(length(res), length(h))
})

#3.
test_that("Matern handles vector inputs correctly", {
  h <- c(0, 0, 0)
  res <- Matern(h, r = 1, v = 0.5)
  expect_equal(res, c(1, 1, 1))
})


#3.
test_that("Matern is symmetric in h (depends on abs |h|)", {
  res_pos <- Matern(c(0.5, 1), r = 2, v = 1)
  res_neg <- Matern(c(-0.5, -1), r = 2, v = 1)
  res_mixed <- Matern(c(-0.5, 1), r = 2, v = 1)
  res_mixed_inversed <- Matern(c(0.5, -1), r = 2, v = 1)
  
  expect_equal(res_pos, res_neg)
  expect_equal(res_mixed, res_mixed_inversed)
  expect_equal(res_pos, res_mixed)
  
})

#4.
test_that("Matern decreases as distance increases", {
  r <- 1
  v <- 0.5
  h <- c(0, 0.5, 1, 2, 5,10,100)
  res <- Matern(h, r, v)
  
  expect_true(isTRUE(all(diff(res) <= 0)))
})

#5.
test_that("Matern with v = 0.5 matches exponential covariance, see the mathematical formula", {
  # La formule Matern v=0.5 = exp(-r*h)
  h <- c(0.1, 0.5, 1, 2)
  r <- 1
  expected <- exp(-r * abs(h))
  res <- Matern(h, r, v = 0.5)
  
  expect_equal(res, expected, tolerance = 1e-6)
})

#6.
test_that("Matern with large h tends to zero", {
  h <- 1000000
  res <- Matern(h, r = 1, v = 1)
  
  expect_lt(res, 1e-6)
})

#7.
test_that("Matern keeps values within [0,1] for h >= 0", {
  h <- seq(0, 5, length.out = 20)
  res <- Matern(h, r = 1, v = 1)
  
  expect_true(all(res >= 0))
  expect_true(all(res <= 1))
})

#8.
test_that("Matern handles non-integer smoothness v", {
  h <- c(0.2, 0.7, 1.5)
  
  res <- Matern(h, r = 1.2, v = 0.3)
    expect_true(all(is.finite(res)))
})

#9.
test_that("Matern returns finite values and no NaN/Inf (except h=0 case)", {
  h <- seq(0, 10, by = 0.1)
  
  res <- Matern(h, r = 1, v = 0.7)
  expect_true(all(is.finite(res)))
})

#10.
test_that("Matern returns finite values and no NaN/Inf (except h=0 case)", {
  h <- 0
  
  res <- Matern(h, r = 1, v = 0.7)
re})

#11.
test_that("Matern with r=0 and h=0 returns 1. to See with JEff as r or v should not be equal to 0", {
  h <- 0
  res <- Matern(h, r = 0, v = 1)
  "A voir avec JEFF"
  expect_equal(res,1)
})


#12.
test_that("Matern with r=0 returns NA or 1 ?? A voir avec JEFF ", {
  h <- c(0.5, 1, 2)
  res <- Matern(h, r = 0, v = 1)

  expect_true(all(is.na(res)))
})

#13.
test_that("Matern throws error or Nan because v should never be equal to 0", {
  h <- c(0.5, 1, 2)
  res <- Matern(h, r = 0.5, v = 0)

  expect_true(all(is.na(res))) 
})

#14.
test_that("Matern throws error for v < 0 or Nan because v should never be inferior to 0 see with Jeff", {
  expect_true(all(Matern(1, r = 1, v = -1))
})


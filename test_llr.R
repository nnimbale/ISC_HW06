context("Check local linear regression function")
source("llr_functions.R")

n = 15
## a very simple regression model
x = rnorm(n)
y = rnorm(x + rnorm(n))
z = seq(-1, 1, length.out = 100)
test_that("llr output has correct length", {
  expect_equal(length(llr(x, y, z, omega = 1)), length(z))
})


test_that("llr output length matches input length", {
  x <- c(1, 2, 3)
  y <- c(4, 5, 6)
  z <- c(7, 8, 9)
  omega <- 1
  fits <- llr(x, y, z, omega)
  expect_equal(length(fits), length(z))
})

test_that("make_weight_matrix returns correct diagonal matrix", {
  x <- c(1, 2, 3)
  z <- 2
  omega <- 1
  W <- make_weight_matrix(z, x, omega)
  expect_true(is.matrix(W) && nrow(W) == length(x))
})

test_that("make_predictor_matrix returns correct X matrix", {
  x <- c(1, 2, 3)
  X <- make_predictor_matrix(x)
  expect_equal(dim(X), c(length(x), 2))
  expect_equal(X[,1], rep(1, length(x)))
})


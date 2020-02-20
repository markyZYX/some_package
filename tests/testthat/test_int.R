
context("integer")

require(testthat)


test_that("single logical small", {
  expect_equal(msort(1:10), 1:10)
})


test_that("single logical medium", {

  x <- as.integer((runif(95e3) - 0.5) * 2e9)
  z <- msort(c(x, 1L))

  expect_equal(z, sort(c(x, 1L)))
})


test_that("single logical large", {

  x <- as.integer((runif(65e4) - 0.5) * 2e9)
  z <- msort(c(x, 1L))
  
  expect_equal(z, sort(c(x, 1L)))
})

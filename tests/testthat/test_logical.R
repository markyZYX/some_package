
context("logical")

require(testthat)


test_that("single logical small", {
  expect_equal(msort(c(TRUE, TRUE, FALSE, FALSE, FALSE, NA)),
    c(NA, FALSE, FALSE, FALSE, TRUE, TRUE))
})


test_that("single logical medium", {

  x <- sample(c(NA, FALSE, TRUE), 131080, replace = TRUE)
  y <- c(x, TRUE, FALSE, NA)
  z <- msort(y)

  expect_equal(z, y)

  # counts are equal
  expect_equal(sum(is.na(x)) + 1, sum(is.na(z)))
  expect_equal(sum(x, na.rm = TRUE) + 1, sum(z, na.rm = TRUE))
  expect_equal(sum(!x) + 1, sum(!z))

  expect_equal(z[1], NA)
  expect_equal(tail(z, 1), TRUE)
})


test_that("single logical large", {

  x <- sample(c(NA, FALSE, TRUE), 1049000, replace = TRUE)
  y <- c(x, TRUE, FALSE, NA)
  z <- msort(y)

  expect_equal(z, y)

  # counts are equal
  expect_equal(sum(is.na(x)) + 1, sum(is.na(z)))
  expect_equal(sum(x, na.rm = TRUE) + 1, sum(z, na.rm = TRUE))
  expect_equal(sum(!x) + 1, sum(!z))

  expect_equal(z[1], NA)
  expect_equal(tail(z, 1), TRUE)
})

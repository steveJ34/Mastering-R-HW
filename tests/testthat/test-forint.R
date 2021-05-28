library(testthat)

context("mr package testing")

test_that("forint function works properly", {
  expect_equal(forint(42), "42 Ft")
})

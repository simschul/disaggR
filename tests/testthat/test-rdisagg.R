library(testthat)

test_that("multiplication works", {
  expect_error(rdisagg(1, mean_0 = 1, shares = c(0.8,0.21)))
  expect_no_error(rdisagg(1, mean_0 = 1, shares = c(0.8,NA)))
})

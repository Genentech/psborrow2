test_that("covariance_matrix works", {
  result <- covariance_matrix(c(1:4), c(1, 4, -2, 1, 0, 0))
  expected <- matrix(c(1, 1, 4, 1, 1, 2, -2, 0, 4, -2, 3, 0, 1, 0, 0, 4), nrow = 4)

  expect_equal(result, expected)
})

test_that("covariance_matrix fails with incompatible arguments", {
  expect_error(covariance_matrix(c(1:3), c(0, 0)))
  expect_error(covariance_matrix(c("a", "b", "c")))
  expect_error(covariance_matrix(c(1:5)))
})

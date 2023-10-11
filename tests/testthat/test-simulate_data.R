test_that("covariance_matrix works", {
  result <- covariance_matrix(c(1:4), c(1, 0, 0, 0, 0, 0))
  expected <- matrix(
    c(
      1, 1, 0, 0,
      1, 2, 0, 0,
      0, 0, 3, 0,
      0, 0, 0, 4
    ),
    nrow = 4
  )

  expect_equal(result, expected)
})

test_that("covariance_matrix finds positive definite matrices", {
  expect_warning(
    result <- covariance_matrix(c(1:4), c(1, 4, -2, 1, 0, 0)),
    "Finding nearest positive definite matrix"
  )
  expected <- matrix(
    c(
      1, -0.173717259157214, 1.55477286166658, 0.606677145265074,
      -0.173717259157214, 2, -1.25191988052734, 0.120331114550728,
      1.55477286166658, -1.25191988052734, 3, 0.250687992911699,
      0.606677145265074, 0.120331114550728, 0.250687992911699, 4
    ),
    nrow = 4
  )

  expect_equal(result, expected)
})


test_that("covariance_matrix fails with incompatible arguments", {
  expect_error(covariance_matrix(c(1:3), c(0, 0)))
  expect_error(covariance_matrix(c("a", "b", "c")))
})

test_that("LogisticBinaryOutcome distribution is rendering correctly", {
  # Binomial endpoint class
  bin_endpoint <- logistic_bin_outcome()

  # Expect correct class
  expect_class(bin_endpoint, "LogisticBinaryOutcome")
  expect_equal(bin_endpoint@n_param, 0L)

  # Errors
  expect_error(logistic_bin_outcome(3),
    regexp = "unused argument"
  )
})

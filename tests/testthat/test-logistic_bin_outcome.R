test_that("LogisticBinaryOutcome distribution is rendering correctly", {
  # Binomial endpoint class
  bin_endpoint <- logistic_bin_outcome(binary_var = "response")

  # Expect correct class
  expect_class(bin_endpoint, "LogisticBinaryOutcome")
  expect_equal(bin_endpoint@n_param, 0L)

  # Errors
  expect_error(logistic_bin_outcome(),
    regexp = 'argument \"binary_var\" is missing, with no default'
  )
})

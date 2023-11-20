test_that("OutcomeBinaryLogistic distribution is rendering correctly", {
  # Binomial endpoint class
  bin_endpoint <- outcome_bin_logistic(
    binary_var = "response",
    baseline_prior = prior_normal(0, 1000)
  )

  # Expect correct class
  expect_class(bin_endpoint, "OutcomeBinaryLogistic")
  expect_equal(bin_endpoint@n_param, 0L)

  # Errors
  expect_error(outcome_bin_logistic(),
    regexp = 'argument \"binary_var\" is missing, with no default'
  )
})

test_that("get_vars works for OutcomeBinaryLogistic", {
  expect_identical(
    get_vars(outcome_bin_logistic(
      binary_var = "response",
      baseline_prior = prior_normal(0, 1000)
    )),
    c(binary_var = "response")
  )

  expect_identical(
    get_vars(outcome_bin_logistic(
      binary_var = "response",
      baseline_prior = prior_normal(0, 1000),
      weight_var = "w"
    )),
    c(binary_var = "response", weight_var = "w")
  )
})

test_that("outcome_bin_logistic works with weights", {
  result <- outcome_bin_logistic(
    binary_var = "response",
    baseline_prior = prior_normal(0, 1000),
    weight_var = "w"
  )
  expect_class(result, "OutcomeBinaryLogistic")
  expect_equal(result@weight_var, "w")
  expect_string(
    result@likelihood_stan_code,
    fixed = "for (i in 1:N) {\n  target += bernoulli_logit_lupmf(y[i] | lp[i]) * weight[i];\n}"
  )
  expect_string(
    result@data_stan_code,
    fixed = "array[N] int y;\nvector[N] weight;"
  )
})


test_that("logistic_bin_outcome() throws error", {
  expect_error(logistic_bin_outcome(),
    regexp = "deprecated"
  )

  expect_error(logistic_bin_outcome(a = 2),
    regexp = "deprecated"
  )
})

test_that("OutcomeContinuousNormal distribution is rendering correctly", {
  # Continuous Normal endpoint class
  cont_endpoint <- outcome_cont_normal(
    continuous_var = "response",
    baseline_prior = prior_normal(0, 1000),
    std_dev_prior = prior_half_cauchy(0, 3)
  )

  # Expect correct class
  expect_class(cont_endpoint, "OutcomeContinuousNormal")
  expect_equal(cont_endpoint@n_param, 0L)

  # Errors
  expect_error(outcome_cont_normal(),
    regexp = 'argument \"continuous_var\" is missing, with no default'
  )
})

test_that("get_vars works for OutcomeContinuousNormal", {
  expect_identical(
    get_vars(outcome_cont_normal(
      continuous_var = "response",
      baseline_prior = prior_normal(0, 1000),
      std_dev_prior = prior_half_cauchy(0, 3)
    )),
    c(continuous_var = "response")
  )

  expect_identical(
    get_vars(outcome_cont_normal(
      continuous_var = "response",
      baseline_prior = prior_normal(0, 1000),
      std_dev_prior = prior_half_cauchy(0, 3),
      weight_var = "w"
    )),
    c(continuous_var = "response", weight_var = "w")
  )
})

test_that("outcome_cont_normal works with weights", {
  result <- outcome_cont_normal(
    continuous_var = "response",
    baseline_prior = prior_normal(0, 1000),
    std_dev_prior = prior_half_cauchy(0, 3),
    weight_var = "w"
  )
  expect_class(result, "OutcomeContinuousNormal")
  expect_equal(result@weight_var, "w")
  expect_string(
    result@likelihood_stan_code,
    fixed = "for (i in 1:N) {\n  target += normal_lupdf(y[i] | lp[i], std_dev_outcome) * weight[i];\n}"
  )
  expect_string(
    result@data_stan_code,
    fixed = "array[N] real y;\nvector[N] weight;"
  )
})

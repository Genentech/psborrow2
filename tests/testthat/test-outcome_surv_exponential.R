test_that("Exponential survival distributions are rendering correctly", {
  # Make exponential survival distribution
  surv_dist <- outcome_surv_exponential(
    time_var = "time",
    cens_var = "cens",
    baseline_prior = prior_normal(0, 1000)
  )

  # Expect correct class
  expect_class(surv_dist, "OutcomeSurvExponential")
  expect_equal(surv_dist@n_param, 0L)
  expect_equal(surv_dist@param_priors, list())

  # Errors
  expect_error(outcome_surv_exponential(),
    regexp = 'argument \"time_var\" is missing, with no default'
  )
})

test_that("get_vars works for OutcomeSurvExponential", {
  expect_identical(
    get_vars(outcome_surv_exponential(
      time_var = "TIME",
      cens_var = "CENS",
      prior_normal(0, 100)
    )),
    c(time_var = "TIME", cens_var = "CENS")
  )

  expect_identical(
    get_vars(outcome_surv_exponential(
      time_var = "TIME",
      cens_var = "CENS",
      weight_var = "W",
      prior_normal(0, 100)
    )),
    c(time_var = "TIME", cens_var = "CENS", weight_var = "W")
  )
})

test_that("outcome_surv_exponential works with weights", {
  result <- outcome_surv_exponential(
    time_var = "time",
    cens_var = "cens",
    prior_normal(0, 1000),
    weight_var = "w"
  )
  expect_class(result, "OutcomeSurvExponential")
  expect_equal(result@weight_var, "w")
  expect_string(
    result@likelihood_stan_code,
    fixed = "for (i in 1:N) {
   if (cens[i] == 1) {
      target += exponential_lccdf(time[i] | elp[i] ) * weight[i];
   } else {
      target += exponential_lpdf(time[i] | elp[i] ) * weight[i];
   }
}"
  )
  expect_string(
    result@data_stan_code,
    fixed = "vector[N] time;\nvector[N] cens;\nvector[N] weight;"
  )
})

test_that("exp_surv_dist() throws error", {
  expect_error(exp_surv_dist(),
    regexp = "deprecated"
  )

  expect_error(exp_surv_dist(a = 2),
    regexp = "deprecated"
  )
})

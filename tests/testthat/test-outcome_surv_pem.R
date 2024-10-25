test_that("Piecewise exponential survival distributions are rendering correctly", {
  # Make PEM distribution
  surv_dist <- outcome_surv_pem(
    time_var = "time",
    cens_var = "cens",
    baseline_prior = prior_normal(0, 1000),
    cut_points = c(10, 15)
  )

  # Expect correct class
  expect_class(surv_dist, "OutcomeSurvPEM")
  expect_equal(surv_dist@n_param, 0L)
  expect_equal(surv_dist@param_priors, list())

  # Errors
  expect_error(outcome_surv_pem(),
    regexp = 'argument \"time_var\" is missing, with no default'
  )
})

test_that("get_vars works for OutcomeSurvPEM", {
  expect_identical(
    get_vars(outcome_surv_pem(
      time_var = "TIME",
      cens_var = "CENS",
      prior_normal(0, 100),
      cut_points = c(10, 15)
    )),
    c(time_var = "TIME", cens_var = "CENS")
  )

  expect_identical(
    get_vars(outcome_surv_pem(
      time_var = "TIME",
      cens_var = "CENS",
      weight_var = "W",
      prior_normal(0, 100),
      cut_points = c(10, 15)
    )),
    c(time_var = "TIME", cens_var = "CENS", weight_var = "W")
  )
})

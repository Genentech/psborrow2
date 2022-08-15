test_that("Exponential survival distributions are rendering correctly", {
  # Make exponential survival distribution
  surv_dist <- exp_surv_dist(time_var = "time", cens_var = "cens")

  # Expect correct class
  expect_class(surv_dist, "ExponentialSurvDist")
  expect_equal(surv_dist@n_param, 0L)
  expect_equal(surv_dist@param_priors, list())

  # Errors
  expect_error(exp_surv_dist(),
    regexp = 'argument \"time_var\" is missing, with no default'
  )
})

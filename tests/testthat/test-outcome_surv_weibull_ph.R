test_that("Weibull survival distributions are rendering correctly", {
  # Make Weibull survival distribution
  surv_dist <- outcome_surv_weibull_ph(
    time_var = "time",
    cens_var = "cens",
    prior_normal(0, 1000),
    prior_normal(0, 1000)
  )

  # Expect correct class
  expect_class(surv_dist, "OutcomeSurvWeibullPH")
  expect_equal(surv_dist@n_param, 1L)
  expect_true(is(surv_dist@param_priors$shape_weibull, "Prior"))

  # Errors
  expect_error(outcome_surv_weibull_ph(time_var = "time"),
    regexp = 'argument \"cens_var\" is missing, with no default'
  )
})

test_that("get_vars works for OutcomeSurvWeibullPH", {
  expect_identical(
    get_vars(outcome_surv_weibull_ph(
      time_var = "TIME",
      cens_var = "CENS",
      prior_normal(0, 1000),
      prior_normal(0, 1000)
    )),
    c(time_var = "TIME", cens_var = "CENS")
  )

  expect_identical(
    get_vars(outcome_surv_weibull_ph(
      time_var = "TIME",
      cens_var = "CENS",
      prior_normal(0, 1000),
      prior_normal(0, 1000),
      weight_var = "W"
    )),
    c(time_var = "TIME", cens_var = "CENS", weight_var = "W")
  )
})

test_that("outcome_surv_weibull_ph works with weights", {
  result <- outcome_surv_weibull_ph(
    time_var = "time",
    cens_var = "cens",
    prior_normal(0, 1000),
    prior_normal(0, 1000),
    weight_var = "w"
  )
  expect_class(result, "OutcomeSurvWeibullPH")
  expect_equal(result@weight_var, "w")
})

test_that("weib_ph_surv_dist() throws error", {
  expect_error(weib_ph_surv_dist(),
    regexp = "deprecated"
  )

  expect_error(weib_ph_surv_dist(a = 2),
    regexp = "deprecated"
  )
})

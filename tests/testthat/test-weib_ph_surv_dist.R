test_that("Weibull survival distributions are rendering correctly", {
  # Make Weibull survival distribution
  surv_dist <- weib_ph_surv_dist(time_var = "time", cens_var = "cens")

  # Expect correct class
  expect_class(surv_dist, "WeibullPHSurvDist")
  expect_equal(surv_dist@n_param, 1L)
  expect_true(is(surv_dist@param_priors$shape_weibull, "Prior"))

  # Errors
  expect_error(weib_ph_surv_dist(time_var = "time"),
    regexp = 'argument \"cens_var\" is missing, with no default'
  )
})

test_that("get_vars works for WeibullPHSurvDist", {
  expect_identical(
    get_vars(weib_ph_surv_dist(time_var = "TIME", cens_var = "CENS")),
    c(time_var = "TIME", cens_var = "CENS")
  )
})

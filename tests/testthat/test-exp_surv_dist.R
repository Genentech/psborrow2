test_that("Exponential survival distributions are rendering correctly", {
  # Make exponential survival distribution
  surv_dist <- exp_surv_dist(
    time_var = "time",
    cens_var = "cens",
    baseline_prior = normal_prior(0, 1000)
  )

  # Expect correct class
  expect_class(surv_dist, "ExponentialSurvDist")
  expect_equal(surv_dist@n_param, 0L)
  expect_equal(surv_dist@param_priors, list())

  # Errors
  expect_error(exp_surv_dist(),
    regexp = 'argument \"time_var\" is missing, with no default'
  )
})

test_that("get_vars works for ExponentialSurvDist", {
  expect_identical(
    get_vars(exp_surv_dist(
      time_var = "TIME",
      cens_var = "CENS",
      normal_prior(0, 100)
    )),
    c(time_var = "TIME", cens_var = "CENS")
  )

  expect_identical(
    get_vars(exp_surv_dist(
      time_var = "TIME",
      cens_var = "CENS",
      weight_var = "W",
      normal_prior(0, 100)
    )),
    c(time_var = "TIME", cens_var = "CENS", weight_var = "W")
  )
})

test_that("exp_surv_dist works with weights", {
  result <- exp_surv_dist(
    time_var = "time",
    cens_var = "cens",
    normal_prior(0, 1000),
    weight_var = "w"
  )
  expect_class(result, "ExponentialSurvDist")
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
})

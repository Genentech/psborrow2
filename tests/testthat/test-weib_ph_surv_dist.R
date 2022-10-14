test_that("Weibull survival distributions are rendering correctly", {
  # Make Weibull survival distribution
  surv_dist <- weib_ph_surv_dist(
    time_var = "time",
    cens_var = "cens",
    normal_prior(0, 1000),
    normal_prior(0, 1000)
  )

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
    get_vars(weib_ph_surv_dist(
      time_var = "TIME",
      cens_var = "CENS",
      normal_prior(0, 1000),
      normal_prior(0, 1000)
    )),
    c(time_var = "TIME", cens_var = "CENS")
  )

  expect_identical(
    get_vars(weib_ph_surv_dist(
      time_var = "TIME",
      cens_var = "CENS",
      normal_prior(0, 1000),
      normal_prior(0, 1000),
      weight_var = "W"
    )),
    c(time_var = "TIME", cens_var = "CENS", weight_var = "W")
  )
})

test_that("weib_ph_surv_dist works with weights", {
  result <- weib_ph_surv_dist(
    time_var = "time",
    cens_var = "cens",
    normal_prior(0, 1000),
    normal_prior(0, 1000),
    weight_var = "w"
  )
  expect_class(result, "WeibullPHSurvDist")
  expect_equal(result@weight_var, "w")
  expect_string(
    result@likelihood_stan_code,
    fixed = "for (i in 1:N) {
   if (cens[i] == 1) {
      target += weibull_ph_lccdf(time[i] | shape_weibull, elp[i] ) * weight[i];
   } else {
      target += weibull_ph_lpdf(time[i] | shape_weibull, elp[i] ) * weight[i];
   }
}"
  )
})

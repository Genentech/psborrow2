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

test_that("outcome_surv_pem works with weights", {
  result <- outcome_surv_pem(
    time_var = "time",
    cens_var = "cens",
    prior_normal(0, 1000),
    weight_var = "w",
    cut_points  = c(10, 15)
  )
  expect_class(result, "OutcomeSurvPEM")
  expect_equal(result@weight_var, "w")
  expect_string(
    result@likelihood_stan_code,
    fixed = "for (i in 1:N) {\n   if (cens[i] == 1) {\n      target += exponential_lccdf(time[i] | elp[i] ) * weight[i];\n   } else {\n      target += exponential_lpdf(time[i] | elp[i] ) * weight[i];\n   }\n}"
  )
  expect_string(
    result@data_stan_code,
    fixed = "vector[N] time;\nvector[N] cens;\nvector[N] weight;"
  )
})


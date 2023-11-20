test_that("make_model_string_model works with exponential survival and full borrowing", {
  object <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_surv_exponential("time", "cnsr", normal_prior(0, 100)),
    borrowing = borrowing_details(
      "Full borrowing",
      "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 1000))
  )

  result <- psborrow2:::make_model_string_model(object)
  expect_class(result, "glue")
  expect_string(fixed = "exponential_lccdf", x = result)
  expect_string(fixed = "alpha + trt * beta_trt", x = result)
  expect_string(fixed = "alpha ~ normal(0, 100)", x = result)
  expect_snapshot(result)
})

test_that("make_model_string_model works with exponential survival and BDB", {
  object <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_surv_exponential("time", "cnsr", normal_prior(0, 100)),
    borrowing = borrowing_details(
      "BDB",
      "ext",
      prior_exponential(0.001)
    ),
    treatment = treatment_details("trt", normal_prior(0, 1000))
  )

  result <- psborrow2:::make_model_string_model(object)
  expect_class(result, "glue")
  expect_string(fixed = "exponential_lccdf", x = result)
  expect_string(fixed = "Z * alpha + trt * beta_trt", x = result)
  expect_string(fixed = "alpha[1]", x = result)
  expect_string(fixed = "alpha[1] ~ normal(alpha[2]", x = result)
  expect_snapshot(result)
})

test_that("make_model_string_model works with weibull survival and BDB and covariates", {
  object <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      normal_prior(0, 1000)
    ),
    outcome = outcome_surv_weibull_ph(
      "time",
      "cnsr",
      normal_prior(0, 1000),
      normal_prior(0, 100)
    ),
    borrowing = borrowing_details(
      "BDB",
      "ext",
      prior_exponential(0.001)
    ),
    treatment = treatment_details("trt", normal_prior(0, 1000))
  )

  result <- psborrow2:::make_model_string_model(object)
  expect_class(result, "glue")
  expect_string(fixed = "weibull_ph_lccdf", x = result)
  expect_string(fixed = "X * beta + Z * alpha + trt * beta_trt", x = result)
  expect_string(fixed = "alpha[2]", x = result)
  expect_string(fixed = "alpha[1]", x = result)
  expect_string(fixed = "alpha[1] ~ normal(alpha[2]", x = result)
  expect_snapshot(result)
})

test_that("make_model_string_model works with binary outcome and BDB and covariates", {
  object <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      normal_prior(0, 1000)
    ),
    outcome = outcome_bin_logistic("cnsr", normal_prior(0, 100)),
    borrowing = borrowing_details(
      "BDB",
      "ext",
      prior_exponential(0.001)
    ),
    treatment = treatment_details("trt", normal_prior(0, 1000))
  )

  result <- psborrow2:::make_model_string_model(object)
  expect_class(result, "glue")
  expect_string(fixed = "bernoulli_logit_lupmf", x = result)
  expect_string(fixed = "X * beta + Z * alpha + trt * beta_trt", x = result)
  expect_string(fixed = "alpha[2]", x = result)
  expect_string(fixed = "alpha[1]", x = result)
  expect_string(fixed = "alpha[1] ~ normal(alpha[2]", x = result)
  expect_snapshot(result)
})

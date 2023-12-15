test_that("make_model_string_parameters works with exponential survival and full borrowing", {
  object <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_surv_exponential("time", "cnsr", prior_normal(0, 100)),
    borrowing = borrowing_full("ext"),
    treatment = treatment_details("trt", prior_normal(0, 1000))
  )

  result <- psborrow2:::make_model_string_parameters(object)
  expect_true(is(result, "glue"))
  expect_snapshot(result)
})

test_that("make_model_string_parameters works with exponential survival and BDB", {
  object <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_surv_exponential("time", "cnsr", prior_normal(0, 100)),
    borrowing = borrowing_hierarchical_commensurate(
      "ext",
      prior_exponential(0.001)
    ),
    treatment = treatment_details("trt", prior_normal(0, 1000))
  )

  result <- psborrow2:::make_model_string_parameters(object)
  expect_true(is(result, "glue"))
  expect_snapshot(result)
})

test_that("make_model_string_parameters works with weibull survival and BDB", {
  object <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      prior_normal(0, 1000)
    ),
    outcome = outcome_surv_weibull_ph(
      "time",
      "cnsr",
      prior_normal(0, 1000),
      prior_normal(0, 100)
    ),
    borrowing = borrowing_hierarchical_commensurate(
      "ext",
      prior_exponential(0.001)
    ),
    treatment = treatment_details("trt", prior_normal(0, 1000))
  )

  result <- psborrow2:::make_model_string_parameters(object)
  expect_true(is(result, "glue"))
  expect_snapshot(result)
})

test_that("make_model_string_parameters works with binary outcome and BDB", {
  object <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      prior_normal(0, 1000)
    ),
    outcome = outcome_bin_logistic("cnsr", prior_normal(0, 100)),
    borrowing = borrowing_hierarchical_commensurate(
      "ext",
      prior_exponential(0.001)
    ),
    treatment = treatment_details("trt", prior_normal(0, 1000))
  )

  result <- psborrow2:::make_model_string_parameters(object)
  expect_true(is(result, "glue"))
  expect_snapshot(result)
})

test_that("the STAN code is correctly generated when limits are placed in the treatment parameters", {
  skip_if_not(check_cmdstan())
  stan_model_string <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_surv_exponential("time", "cnsr", prior_normal(0, 100000)),
    borrowing = borrowing_full("ext"),
    treatment = treatment_details("trt", prior_half_cauchy(0, 20))
  )@model_string

  expect_true(grepl("real<lower=0> beta_trt", stan_model_string))
})

test_that("the STAN code is correctly generated when limits are placed in the borrowing parameters", {
  skip_if_not(check_cmdstan())
  stan_model_string_cauchy <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_surv_exponential("time", "cnsr", prior_normal(0, 100000)),
    borrowing = borrowing_hierarchical_commensurate(
      ext_flag_col = "ext",
      tau_prior = prior_half_cauchy(10, 20)
    ),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )@model_string

  stan_model_string_normal <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_surv_exponential("time", "cnsr", prior_normal(0, 100000)),
    borrowing = borrowing_hierarchical_commensurate(
      ext_flag_col = "ext",
      tau_prior = prior_half_normal(10, 20)
    ),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )@model_string

  expect_true(grepl("real<lower=10> tau", stan_model_string_cauchy))
  expect_true(grepl("real<lower=10> tau", stan_model_string_normal))
})

test_that("the STAN code is correctly generated when limits are placed in the outcome parameters", {
  skip_if_not(check_cmdstan())
  stan_model_string <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_surv_exponential("time", "cnsr", uniform_prior(0, 10)),
    borrowing = borrowing_full("ext"),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )@model_string

  expect_true(grepl("real<lower=0,upper=10> alpha;", stan_model_string))
})

test_that("the STAN code is correctly generated when limits are placed in the outcome parameters", {
  skip_if_not(check_cmdstan())
  stan_model_string <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_surv_exponential("time", "cnsr", uniform_prior(0, 10)),
    borrowing = borrowing_full("ext"),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )@model_string

  expect_true(grepl("real<lower=0,upper=10> alpha", stan_model_string))
})


test_that("the STAN code is correctly generated when limits are placed in the covariate parameters", {
  skip_if_not(check_cmdstan())
  anls_obj <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates("cov1", uniform_prior(0, 10)),
    outcome = outcome_surv_exponential("time", "cnsr", prior_normal(0, 10000)),
    borrowing = borrowing_full("ext"),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )

  inputs <- psborrow2:::prepare_stan_data_inputs(anls_obj)

  expect_equal(inputs$L_beta, c("lower" = 0))
  expect_equal(inputs$U_beta, c("upper" = 10))

  anls_obj <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c(
        "cov1",
        "cov2"
      ),
      list(
        uniform_prior(0, 10),
        uniform_prior(10, 20)
      )
    ),
    outcome = outcome_surv_exponential("time", "cnsr", prior_normal(0, 10000)),
    borrowing = borrowing_full("ext"),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )

  inputs <- psborrow2:::prepare_stan_data_inputs(anls_obj)

  expect_equal(inputs$L_beta, c(0, 10))
  expect_equal(inputs$U_beta, c(10, 20))
})

test_that("make_model_string_parameters works with exponential survival and full borrowing", {
  object <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    outcome = exp_surv_dist("time", "cnsr"),
    borrowing = borrowing_details(
      "Full borrowing",
      normal_prior(0, 100),
      "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 1000))
  )

  result <- psborrow2:::make_model_string_parameters(object)
  expect_true(is(result, "glue"))
  expect_snapshot(result)
})

test_that("make_model_string_parameters works with exponential survival and BDB", {
  object <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    outcome = exp_surv_dist("time", "cnsr"),
    borrowing = borrowing_details(
      "BDB",
      normal_prior(0, 100),
      "ext",
      exponential_prior(0.001)
    ),
    treatment = treatment_details("trt", normal_prior(0, 1000))
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
      normal_prior(0, 1000)
    ),
    outcome = weib_ph_surv_dist("time", "cnsr", normal_prior(0, 1000)),
    borrowing = borrowing_details(
      "BDB",
      normal_prior(0, 100),
      "ext",
      exponential_prior(0.001)
    ),
    treatment = treatment_details("trt", normal_prior(0, 1000))
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
      normal_prior(0, 1000)
    ),
    outcome = logistic_bin_outcome("cnsr"),
    borrowing = borrowing_details(
      "BDB",
      normal_prior(0, 100),
      "ext",
      exponential_prior(0.001)
    ),
    treatment = treatment_details("trt", normal_prior(0, 1000))
  )

  result <- psborrow2:::make_model_string_parameters(object)
  expect_true(is(result, "glue"))
  expect_snapshot(result)
})

test_that("make_model_string_functions works with exponential survival and full borrowing", {
  object <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_surv_exponential("time", "cnsr", prior_normal(0, 1000)),
    borrowing = borrowing_full("ext"),
    treatment = treatment_details("trt", prior_normal(0, 1000))
  )

  result <- psborrow2:::make_model_string_functions(object)
  expect_class(result, "glue")
  expect_string(fixed = "functions", x = result)
  expect_snapshot(result)
})

test_that("make_model_string_functions works with exponential survival and BDB", {
  object <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_surv_exponential("time", "cnsr", prior_normal(0, 1000)),
    borrowing = borrowing_hierarchical_commensurate(
      "ext",
      prior_exponential(0.001)
    ),
    treatment = treatment_details("trt", prior_normal(0, 1000))
  )

  result <- psborrow2:::make_model_string_functions(object)
  expect_class(result, "glue")
  expect_string(fixed = "functions", x = result)
  expect_snapshot(result)
})

test_that("make_model_string_functions works with weibull survival and BDB and covariates", {
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
      prior_normal(0, 1000)
    ),
    borrowing = borrowing_hierarchical_commensurate(
      "ext",
      prior_exponential(0.001)
    ),
    treatment = treatment_details("trt", prior_normal(0, 1000))
  )

  result <- psborrow2:::make_model_string_functions(object)
  expect_class(result, "glue")
  expect_string(fixed = "functions", x = result)
  expect_string(fixed = "real", x = result)
  expect_string(fixed = "weibull_ph_lpdf", x = result)
  expect_string(fixed = "weibull_ph_lcdf", x = result)
  expect_string(fixed = "weibull_ph_lccdf", x = result)
  expect_snapshot(result)
})

test_that("make_model_string_functions works with binary outcome and BDB and covariates", {
  object <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      prior_normal(0, 1000)
    ),
    outcome = outcome_bin_logistic("cnsr", prior_normal(0, 1000)),
    borrowing = borrowing_hierarchical_commensurate(
      "ext",
      prior_exponential(0.001)
    ),
    treatment = treatment_details("trt", prior_normal(0, 1000))
  )

  result <- psborrow2:::make_model_string_functions(object)
  expect_class(result, "glue")
  expect_string(fixed = "functions", x = result)
  expect_snapshot(result)
})

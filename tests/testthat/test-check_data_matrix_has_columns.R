test_that("check_data_matrix() catches errors", {
  anls_full <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(c("cov1", "cov2"), prior_normal(0, 1000)),
    outcome = outcome_surv_exponential(
      time_var = "time",
      cens_var = "cnsr",
      prior_normal(0, 1000)
    ),
    treatment = treatment_details(
      "trt",
      prior_normal(0, 1000)
    ),
    borrowing = borrowing_full("ext")
  )

  expect_null(psborrow2:::check_data_matrix_has_columns(anls_full))

  anls_broken <- anls_full
  anls_broken@covariates <- add_covariates(c("cov1", "cov9"), prior_normal(0, 1000))
  expect_error(
    psborrow2:::check_data_matrix_has_columns(anls_broken),
    "The following specified variables were not found in `data_matrix`:\n  covariates: cov9"
  )

  anls_broken <- anls_full
  anls_broken@treatment <- treatment_details("trt_flag", prior_normal(0, 1000))
  expect_error(
    psborrow2:::check_data_matrix_has_columns(anls_broken),
    "The following specified variables were not found in `data_matrix`:\n  trt_flag_col: trt_flag"
  )

  anls_broken <- anls_full
  anls_broken@outcome <- outcome_surv_exponential(
    time_var = "time_months",
    cens_var = "non_dead",
    baseline_prior = prior_normal(0, 1000)
  )
  expect_error(
    psborrow2:::check_data_matrix_has_columns(anls_broken),
    "The following specified variables were not found in `data_matrix`:\n  time_var: time_months\n  cens_var: non_dead"
  )

  anls_broken <- anls_full
  anls_broken@outcome <- outcome_surv_exponential(
    time_var = "time",
    cens_var = "cnsr",
    baseline_prior = prior_normal(0, 1000),
    weight_var = "weights"
  )
  expect_error(
    psborrow2:::check_data_matrix_has_columns(anls_broken),
    "The following specified variables were not found in `data_matrix`:\n  weight_var: weights"
  )
})

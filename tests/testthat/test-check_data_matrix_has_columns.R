test_that("check_data_matrix() catches errors", {
  anls_full <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(c("cov1", "cov2"), normal_prior(0, 1000)),
    outcome = exp_surv_dist(
      time_var = "time",
      cens_var = "cnsr",
      normal_prior(0, 1000)
    ),
    treatment = treatment_details(
      "trt",
      normal_prior(0, 1000)
    ),
    borrowing = borrowing_details(
      "Full borrowing",
      ext_flag_col = "ext"
    )
  )

  expect_null(psborrow2:::check_data_matrix_has_columns(anls_full))

  anls_broken <- anls_full
  anls_broken@covariates <- add_covariates(c("cov1", "cov9"), normal_prior(0, 1000))
  expect_error(
    psborrow2:::check_data_matrix_has_columns(anls_broken),
    "The following specified variables were not found in `data_matrix`:\n  covariates: cov9"
  )

  anls_broken <- anls_full
  anls_broken@treatment <- treatment_details("trt_flag", normal_prior(0, 1000))
  expect_error(
    psborrow2:::check_data_matrix_has_columns(anls_broken),
    "The following specified variables were not found in `data_matrix`:\n  trt_flag_col: trt_flag"
  )

  anls_broken <- anls_full
  anls_broken@outcome <- exp_surv_dist(
    time_var = "time_months",
    cens_var = "non_dead",
    baseline_prior = normal_prior(0, 1000)
  )
  expect_error(
    psborrow2:::check_data_matrix_has_columns(anls_broken),
    "The following specified variables were not found in `data_matrix`:\n  time_var: time_months\n  cens_var: non_dead"
  )
})

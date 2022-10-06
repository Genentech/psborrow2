test_that("get_vars works for Analysis", {
  object <- .analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      normal_prior(0, 1000)
    ),
    outcome = exp_surv_dist(
      time_var = "time",
      cens_var = "cnsr",
      baseline_prior = normal_prior(0, 1000)
    ),
    treatment = treatment_details(
      "trt",
      normal_prior(0, 1000)
    ),
    borrowing = borrowing_details(
      method = "Full borrowing",
      ext_flag_col = "ext"
    )
  )
  expect_equal(
    get_vars(object),
    c(time_var = "time", cens_var = "cnsr", ext_flag_col = "ext", trt_flag_col = "trt", "cov1", "cov2")
  )

  object@covariates <- NULL
  expect_equal(
    get_vars(object),
    c(time_var = "time", cens_var = "cnsr", ext_flag_col = "ext", trt_flag_col = "trt")
  )
})

test_that("show works for Analysis", {
  object <- .analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      normal_prior(0, 1000)
    ),
    outcome = exp_surv_dist(
      time_var = "time",
      cens_var = "cnsr",
      baseline_prior = normal_prior(0, 1000)
    ),
    treatment = treatment_details(
      "trt",
      normal_prior(0, 1000)
    ),
    borrowing = borrowing_details(
      method = "Full borrowing",
      ext_flag_col = "ext"
    )
  )
  expect_snapshot(show(object))
})

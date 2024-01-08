test_that("get_vars works for Analysis", {
  object <- .analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      prior_normal(0, 1000)
    ),
    outcome = outcome_surv_exponential(
      time_var = "time",
      cens_var = "cnsr",
      baseline_prior = prior_normal(0, 1000)
    ),
    treatment = treatment_details(
      "trt",
      prior_normal(0, 1000)
    ),
    borrowing = borrowing_full("ext")
  )
  expect_equal(
    get_vars(object),
    c(time_var = "time", cens_var = "cnsr", ext_flag_col = "ext", trt_flag_col = "trt", "cov1", "cov2")
  )

  object@covariates <- no_covariates()
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
      prior_normal(0, 1000)
    ),
    outcome = outcome_surv_exponential(
      time_var = "time",
      cens_var = "cnsr",
      baseline_prior = prior_normal(0, 1000)
    ),
    treatment = treatment_details(
      "trt",
      prior_normal(0, 1000)
    ),
    borrowing = borrowing_full("ext")
  )
  expect_snapshot(show(object))
})

test_that("show works without covariates", {
  object <- .analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_surv_exponential(
      time_var = "time",
      cens_var = "cnsr",
      baseline_prior = prior_normal(0, 1000)
    ),
    treatment = treatment_details(
      "trt",
      prior_normal(0, 1000)
    ),
    borrowing = borrowing_full("ext")
  )
  expect_snapshot(show(object))
})


test_that("show works with no borrowing", {
  object <- .analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_surv_exponential(
      time_var = "time",
      cens_var = "cnsr",
      baseline_prior = prior_normal(0, 1000)
    ),
    treatment = treatment_details(
      "trt",
      prior_normal(0, 1000)
    ),
    borrowing = borrowing_none(
      ext_flag_col = "ext"
    )
  )
  expect_snapshot(show(object))
})

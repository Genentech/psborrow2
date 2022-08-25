test_that("data matrix trimming works", {
  anls1 <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(covariates = "cov1", normal_prior(0, 1000)),
    outcome = exp_surv_dist("time", "cnsr"),
    treatment = treatment_details("trt", trt_prior = normal_prior(0, 1000)),
    borrowing = borrowing_details(
      method = "BDB",
      baseline_prior = normal_prior(0, 1000),
      ext_flag_col = "ext",
      tau_prior = exponential_prior(0.001)
    )
  )

  anls2 <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    outcome = exp_surv_dist("time", "cnsr"),
    treatment = treatment_details("trt", trt_prior = normal_prior(0, 1000)),
    borrowing = borrowing_details(
      method = "Full borrowing",
      baseline_prior = normal_prior(0, 1000),
      ext_flag_col = "ext"
    )
  )

  anls1_trim <- psborrow2:::trim_data_matrix(anls1)
  anls2_trim <- psborrow2:::trim_data_matrix(anls2)

  expect_set_equal(
    colnames(anls1_trim@data_matrix), c("time", "cnsr", "cov1", "trt", "ext")
  )

  expect_set_equal(
    colnames(anls2_trim@data_matrix), c("time", "cnsr", "trt")
  )
})

test_that("data matrix trimming works with BDB", {
  anls1 <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(covariates = "cov1", normal_prior(0, 1000)),
    outcome = exp_surv_dist("time", "cnsr", normal_prior(0, 1000)),
    treatment = treatment_details("trt", trt_prior = normal_prior(0, 1000)),
    borrowing = borrowing_details(
      method = "BDB_HCP",
      ext_flag_col = "ext",
      tau_prior = exponential_prior(0.001)
    )
  )

  anls1_trim <- psborrow2:::trim_data_matrix(anls1)
  expect_matrix(anls1_trim, mode = "numeric", nrows = 500, ncols = 5)
  expect_set_equal(
    colnames(anls1_trim), c("time", "cnsr", "cov1", "trt", "ext")
  )
})

test_that("data matrix trimming works with Full Borrowing", {
  anls2 <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    outcome = exp_surv_dist("time", "cnsr", normal_prior(0, 1000)),
    treatment = treatment_details("trt", trt_prior = normal_prior(0, 1000)),
    borrowing = borrowing_details(
      method = "Full borrowing",
      ext_flag_col = "ext"
    )
  )

  anls2_trim <- psborrow2:::trim_data_matrix(anls2)
  expect_matrix(anls2_trim, mode = "numeric", nrows = 500, ncols = 3)
  expect_set_equal(
    colnames(anls2_trim), c("time", "cnsr", "trt")
  )
})

test_that("data matrix trimming works with No Borrowing", {
  object <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    outcome = exp_surv_dist("time", "cnsr", normal_prior(0, 1000)),
    treatment = treatment_details("trt", trt_prior = normal_prior(0, 1000)),
    covariates = add_covariates("cov1", normal_prior(0, 1000)),
    borrowing = borrowing_details(
      method = "No borrowing",
      ext_flag_col = "ext"
    )
  )

  result <- psborrow2:::trim_data_matrix(object)
  expect_matrix(result, mode = "numeric", nrows = 150, ncols = 4)
  expect_set_equal(
    colnames(result), c("time", "cnsr", "trt", "cov1")
  )
})

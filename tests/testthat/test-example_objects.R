test_that("example_analysis_object works", {
  result <- example_analysis_object(1)
  expected <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      covariates = c("cov1", "cov2"),
      priors = normal_prior(0, 1000)
    ),
    outcome = exp_surv_dist(
      "time",
      "cnsr",
      baseline_prior = normal_prior(0, 1000)
    ),
    borrowing = borrowing_details(
      "BDB",
      "ext",
      exponential_prior(.001)
    ),
    treatment = treatment_details(
      "trt",
      normal_prior(0, 1000)
    )
  )
  expect_identical(result, expected)
})

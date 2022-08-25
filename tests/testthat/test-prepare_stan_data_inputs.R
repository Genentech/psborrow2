test_that("prepare_stan_data_inputs works with exponential survival and full borrowing", {
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

  result <- psborrow2:::prepare_stan_data_inputs(object)
  expect_list(result, types = "numeric", len = 4)
  expect_equal(names(result), c("N", "trt", "time", "cens"))
})

test_that("prepare_stan_data_inputs works with exponential survival and BDB", {
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

  result <- psborrow2:::prepare_stan_data_inputs(object)
  expect_list(result, types = "numeric", len = 5)
  expect_equal(names(result), c("N", "trt", "time", "cens", "Z"))
})

test_that("prepare_stan_data_inputs works with weibull survival and BDB  and covariates", {
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

  result <- psborrow2:::prepare_stan_data_inputs(object)
  expect_list(result, types = "numeric", len = 7)
  expect_equal(names(result), c("N", "trt", "time", "cens", "Z", "K", "X"))
})

test_that("prepare_stan_data_inputs works with binary outcome and BDB and covariates", {
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

  result <- psborrow2:::prepare_stan_data_inputs(object)
  expect_list(result, types = "numeric", len = 6)
  expect_equal(names(result), c("N", "trt", "y", "Z", "K", "X"))
})

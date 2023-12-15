test_that("prepare_stan_data_inputs works with exponential survival and full borrowing", {
  object <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_surv_exponential("time", "cnsr", prior_normal(0, 100)),
    borrowing = borrowing_full("ext"),
    treatment = treatment_details("trt", prior_normal(0, 1000))
  )

  result <- psborrow2:::prepare_stan_data_inputs(object)
  expect_list(result, types = "numeric", len = 4)
  expect_equal(names(result), c("N", "trt", "time", "cens"))
})

test_that("prepare_stan_data_inputs works with exponential survival and BDB", {
  object <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_surv_exponential("time", "cnsr", prior_normal(0, 100)),
    borrowing = borrowing_hierarchical_commensurate(
      "ext",
      prior_exponential(0.001)
    ),
    treatment = treatment_details("trt", prior_normal(0, 1000))
  )

  result <- psborrow2:::prepare_stan_data_inputs(object)
  expect_list(result, types = "numeric", len = 5)
  expect_equal(names(result), c("N", "trt", "time", "cens", "Z"))
})

test_that("prepare_stan_data_inputs works with weibull survival and BDB and covariates", {
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

  result <- psborrow2:::prepare_stan_data_inputs(object)
  expect_list(result, types = "numeric", len = 9)
  expect_equal(names(result), c("N", "trt", "time", "cens", "Z", "K", "X", "L_beta", "U_beta"))
})

test_that("prepare_stan_data_inputs works with binary outcome and BDB and covariates", {
  object <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      prior_normal(0, 1000)
    ),
    outcome = outcome_bin_logistic("cnsr", prior_normal(0, 100)),
    borrowing = borrowing_hierarchical_commensurate(
      "ext",
      prior_exponential(0.001)
    ),
    treatment = treatment_details("trt", prior_normal(0, 1000))
  )

  result <- psborrow2:::prepare_stan_data_inputs(object)
  expect_list(result, types = "numeric", len = 8)
  expect_equal(names(result), c("N", "trt", "y", "Z", "K", "X", "L_beta", "U_beta"))
})

test_that("prepare_stan_data_inputs returns correct matrix dimensions for X", {
  object1 <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1"),
      prior_normal(0, 1000)
    ),
    outcome = outcome_bin_logistic("cnsr", prior_normal(0, 100)),
    borrowing = borrowing_hierarchical_commensurate(
      "ext",
      prior_exponential(0.001)
    ),
    treatment = treatment_details("trt", prior_normal(0, 1000))
  )

  result1 <- psborrow2:::prepare_stan_data_inputs(object1)
  expect_matrix(result1$X)
  expect_equal(dim(result1$X), c(NROW(example_matrix), 1))

  object2 <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      prior_normal(0, 1000)
    ),
    outcome = outcome_bin_logistic("cnsr", prior_normal(0, 100)),
    borrowing = borrowing_hierarchical_commensurate(
      "ext",
      prior_exponential(0.001)
    ),
    treatment = treatment_details("trt", prior_normal(0, 1000))
  )

  result2 <- psborrow2:::prepare_stan_data_inputs(object2)
  expect_matrix(result2$X)
  expect_equal(dim(result2$X), c(NROW(example_matrix), 2))
})


test_that("prepare_stan_data_inputs works with weights", {
  weights <- seq(1:500 / 500)
  object <- psborrow2:::.analysis_obj(
    data_matrix = cbind(example_matrix, w = weights),
    outcome = outcome_surv_exponential("time", "cnsr", prior_normal(0, 100), weight_var = "w"),
    borrowing = borrowing_full("ext"),
    treatment = treatment_details("trt", prior_normal(0, 1000))
  )

  result <- psborrow2:::prepare_stan_data_inputs(object)
  expect_list(result, types = "numeric", len = 5)
  expect_equal(names(result), c("N", "trt", "time", "cens", "weight"))
  expect_equal(result[["weight"]], weights)
})

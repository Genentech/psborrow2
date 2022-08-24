dat <- survival::diabetic
dat$ext <- dat$trt == 0 & dat$id > 1000
data_mat <- create_data_matrix(
  dat,
  outcome = c("time", "status"),
  trt_flag_col = "trt",
  ext_flag_col = "ext",
  covariates = ~ age + laser
)

anls_1 <- psborrow2:::.analysis_obj(
  data_matrix = data_mat,
  outcome = exp_surv_dist("time", "status"),
  borrowing = borrowing_details(
    "Full borrowing",
    normal_prior(0, 100),
    "extTRUE"
  ),
  treatment = treatment_details("trt", normal_prior(0, 1000))
)

anls_2 <- psborrow2:::.analysis_obj(
  data_matrix = data_mat,
  outcome = exp_surv_dist("time", "status"),
  borrowing = borrowing_details(
    "BDB",
    normal_prior(0, 100),
    "extTRUE",
    exponential_prior(0.001)
  ),
  treatment = treatment_details("trt", normal_prior(0, 1000))
)

anls_3 <- psborrow2:::.analysis_obj(
  data_matrix = data_mat,
  covariates = add_covariates(
    c("age", "laserargon"),
    normal_prior(0, 1000)
  ),
  outcome = weib_ph_surv_dist("time", "status", normal_prior(0, 1000)),
  borrowing = borrowing_details(
    "BDB",
    normal_prior(0, 100),
    "extTRUE",
    exponential_prior(0.001)
  ),
  treatment = treatment_details("trt", normal_prior(0, 1000))
)

anls_4 <- psborrow2:::.analysis_obj(
  data_matrix = data_mat,
  covariates = add_covariates(
    c("age", "laserargon"),
    normal_prior(0, 1000)
  ),
  outcome = logistic_bin_outcome("status"),
  borrowing = borrowing_details(
    "BDB",
    normal_prior(0, 100),
    "extTRUE",
    exponential_prior(0.001)
  ),
  treatment = treatment_details("trt", normal_prior(0, 1000))
)

test_that("prepare_stan_data_inputs appears to function correctly", {
  data_in1 <- psborrow2:::prepare_stan_data_inputs(anls_1)
  data_in2 <- psborrow2:::prepare_stan_data_inputs(anls_2)
  data_in3 <- psborrow2:::prepare_stan_data_inputs(anls_3)
  data_in4 <- psborrow2:::prepare_stan_data_inputs(anls_4)

  expect_true(is(data_in1, "list"))
  expect_true(is(data_in2, "list"))
  expect_true(is(data_in3, "list"))
  expect_true(is(data_in4, "list"))

  expect_equal(
    names(data_in1),
    c("N", "trt", "time", "cens")
  )

  expect_equal(
    names(data_in2),
    c("N", "trt", "time", "cens", "Z")
  )

  expect_equal(
    names(data_in3),
    c("N", "trt", "time", "cens", "Z", "K", "X")
  )

  expect_equal(
    names(data_in4),
    c("N", "trt", "y", "Z", "K", "X")
  )
})

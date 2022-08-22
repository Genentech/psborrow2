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
  borrowing = borrowing_details("Full borrowing", normal_prior(0, 100), "ext"),
  treatment = treatment_details("trt", normal_prior(0, 1000))
)

anls_2 <- psborrow2:::.analysis_obj(
  data_matrix = data_mat,
  outcome = exp_surv_dist("time", "status"),
  borrowing = borrowing_details(
    "BDB",
    normal_prior(0, 100),
    "ext",
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
    "ext",
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
    "ext",
    exponential_prior(0.001)
  ),
  treatment = treatment_details("trt", normal_prior(0, 1000))
)

test_that("prepare_stan_data_inputs appears to function correctly", {
  data_in1 <- psborrow2:::prepare_stan_data_inputs(anls_1)
  data_in2 <- psborrow2:::prepare_stan_data_inputs(anls_2)
  data_in3 <- psborrow2:::prepare_stan_data_inputs(anls_3)
  data_in4 <- psborrow2:::prepare_stan_data_inputs(anls_4)

  expect_true(is(model_str1, "glue"))
  expect_true(is(model_str2, "glue"))
  expect_true(is(model_str3, "glue"))
  expect_true(is(model_str4, "glue"))

  expect_true(grepl("exponential_lccdf", model_str1))
  expect_true(grepl("alpha \\+ trt \\* beta_trt", model_str1))
  expect_true(grepl("alpha \\~ normal\\(0, 100\\)", model_str1))
  expect_false(grepl("alpha\\[2\\]", model_str1))
  expect_false(grepl("alpha\\[1\\]", model_str1))

  expect_true(grepl("exponential_lccdf", model_str2))
  expect_true(grepl("Z \\* alpha \\+ trt \\* beta_trt", model_str2))
  expect_true(grepl("alpha\\[2\\]", model_str2))
  expect_true(grepl("alpha\\[1\\]", model_str2))
  expect_true(grepl("alpha\\[1\\] \\~ normal\\(alpha\\[2\\]", model_str2))

  expect_true(grepl("weibull_ph_lccdf", model_str3))
  expect_true(grepl("X \\* beta \\+ Z \\* alpha \\+ trt \\* beta_trt", model_str3))
  expect_true(grepl("alpha\\[2\\]", model_str3))
  expect_true(grepl("alpha\\[1\\]", model_str3))
  expect_true(grepl("alpha\\[1\\] \\~ normal\\(alpha\\[2\\]", model_str3))

  expect_true(grepl("bernoulli_logit_lupmf", model_str4))
  expect_true(grepl("X \\* beta \\+ Z \\* alpha \\+ trt \\* beta_trt", model_str4))
  expect_true(grepl("alpha\\[2\\]", model_str4))
  expect_true(grepl("alpha\\[1\\]", model_str4))
  expect_true(grepl("alpha\\[1\\] \\~ normal\\(alpha\\[2\\]", model_str4))
})

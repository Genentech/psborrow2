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

test_that("make_model_string_data appears to function correctly", {
  data_str1 <- psborrow2:::make_model_string_data(anls_1)
  data_str2 <- psborrow2:::make_model_string_data(anls_2)
  data_str3 <- psborrow2:::make_model_string_data(anls_3)
  data_str4 <- psborrow2:::make_model_string_data(anls_4)

  expect_true(is(data_str1, "glue"))
  expect_true(is(data_str2, "glue"))
  expect_true(is(data_str3, "glue"))
  expect_true(is(data_str4, "glue"))

  expect_true(grepl("data", data_str1))
  expect_true(grepl("time", data_str1))
  expect_true(grepl("cens", data_str1))
  expect_true(grepl("trt", data_str1))
  expect_false(grepl("X", data_str1))
  expect_false(grepl("y", data_str1))

  expect_true(grepl("data", data_str2))
  expect_true(grepl("time", data_str2))
  expect_true(grepl("cens", data_str2))
  expect_true(grepl("trt", data_str2))
  expect_true(grepl("Z", data_str2))
  expect_false(grepl("X", data_str2))
  expect_false(grepl("y", data_str2))

  expect_true(grepl("data", data_str3))
  expect_true(grepl("time", data_str3))
  expect_true(grepl("cens", data_str3))
  expect_true(grepl("trt", data_str3))
  expect_true(grepl("Z", data_str3))
  expect_true(grepl("X", data_str3))
  expect_false(grepl("y", data_str3))

  expect_true(grepl("data", data_str4))
  expect_false(grepl("time", data_str4))
  expect_false(grepl("cens", data_str4))
  expect_true(grepl("trt", data_str4))
  expect_true(grepl("Z", data_str4))
  expect_true(grepl("X", data_str4))
  expect_true(grepl("y", data_str4))
})
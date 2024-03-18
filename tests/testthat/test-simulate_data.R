# DataSimFixedExternalData ----
test_that("check_fixed_external_data works as expected", {
  set.seed(123)
  data <- data.frame(
    id = 1:20,
    age = rnorm(20, 50, 5),
    score = rpois(20, 10),
    eventtime = rexp(20, 1 / 20),
    status = rbinom(20, 1, 0.8),
    ext = 1,
    trt = 0
  )
  result <- check_fixed_external_data(data, req_cols = c("age", "score"))
  expect_class(result, "DataSimFixedExternalData")

  result_no_flags <- check_fixed_external_data(data[, 1:5], req_cols = c("age", "score"))
  expect_class(result_no_flags, "DataSimFixedExternalData")

  expect_error(check_fixed_external_data(data[, c(1, 3:5)], req_cols = c("age", "score")), "Columns required")
  expect_error(check_fixed_external_data(within(data, ext <- 0), req_cols = "age"), "`ext` values")
  expect_error(check_fixed_external_data(within(data, trt <- 1), req_cols = "age"), "`trt` values")
  expect_error(check_fixed_external_data(within(data, eventtime <- NULL), req_cols = "age"), "column `eventtime`")
  expect_error(check_fixed_external_data(within(data, status <- NULL), req_cols = "age"), "column `status`")
})

# create_event_dist ----
test_that("create_event_dist works", {
  result <- create_event_dist(dist = "weibull", lambdas = 1.1, gammas = 0.9)
  expect_class(result, "DataSimEvent")
  set.seed(99)
  gen_data <- do.call(simsurv::simsurv, c(list(x = data.frame(id = 1:10)), result@params))
  expect_data_frame(gen_data, nrows = 10, ncols = 3, any.missing = FALSE)
  expect_equal(
    gen_data$eventtime,
    c(
      1.34285317175713, 2.45011438227126, 0.030285230572182, 0.880064596397448,
      1.58955110384185, 1.28253549768473, 1.49447380407981, 0.596184415973742,
      0.0877234919454852, 0.866956772649554
    )
  )
})

test_that("create_event_dist works with custom log hazard", {
  loghaz <- function(t, x, betas, ...) (-1 + 0.02 * t - 0.03 * t^2 + 0.005 * t^3)
  result <- create_event_dist(loghazard = loghaz)
  expect_class(result, "DataSimEvent")
  set.seed(99)
  gen_data <- do.call(simsurv::simsurv, c(list(x = data.frame(id = 1:10)), result@params))
  expect_data_frame(gen_data, nrows = 10, ncols = 3, any.missing = FALSE)
  expect_equal(
    gen_data$eventtime,
    c(
      4.05780995025121, 6.70984562155907, 0.128321383518438, 2.72302459351723,
      4.7401016488932, 3.88660961336116, 4.48116498640035, 1.89303597653449,
      0.33382508210794, 2.68481471097052
    )
  )
})

# null_event_dist ----
test_that("null_event_dist works as expected", {
  result <- null_event_dist()
  expect_class(result, "DataSimEvent")
  expect_list(result@params, len = 0)
  expect_equal(result@label, "No distribution specified")
})

# custom_enrollment -------
test_that("custom_enrollment constructor works as expected", {
  result <- custom_enrollment(
    fun = function(n) rpois(n, lambda = 5),
    label = "Poisson enrollment distribution"
  )
  expect_class(result, "DataSimEnrollment")
  expect_string(result@label, "Poisson enrollment distribution")
  set.seed(100)
  expect_equal(result@fun(5), c(4, 3, 5, 2, 5))
})

test_that("custom_enrollment constructor fails as expected", {
  expect_error(
    custom_enrollment(fun = function(num) rpois(num, lambda = 5), label = "test"),
    "formal arguments"
  )
  expect_error(
    custom_enrollment(fun = function(n) rpois(n, lambda = 5), label = NA),
    "label"
  )
})

# enrollment_constant ------

test_that("enrollment_constant works as expected", {
  enrollment <- enrollment_constant(rate = c(3, 2, 1), for_time = c(2, 3, 4))
  expect_class(enrollment, "DataSimEnrollment")
  expect_equal(enrollment@fun(16), c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 6L, 7L, 8L, 9L))
  expect_error(enrollment@fun(100), "Not enough patients")
  expect_error(enrollment@fun(), "argument")
})

test_that("enrollment_constant works with a single rate", {
  enrollment <- enrollment_constant(rate = 5, for_time = 5)
  expect_class(enrollment, "DataSimEnrollment")
  expect_equal(enrollment@fun(12), c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 3L, 3L))
  expect_error(enrollment@fun(100), "Not enough patients")
  expect_error(enrollment@fun(), "argument")
})

test_that("enrollment_constant works with missing for_time", {
  enrollment <- enrollment_constant(rate = c(3, 3, 3, 3, 3, 3, 3))
  expect_class(enrollment, "DataSimEnrollment")
  expect_equal(enrollment@fun(9), c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L))
  expect_equal(enrollment@fun(21), rep(1:7, each = 3))
  expect_error(enrollment@fun(22), "Not enough")
})


# set_enrollment -------
test_that("set_enrollment works", {
  baseline <- create_baseline_object(
    n_trt_int = 50,
    n_ctrl_int = 40,
    n_ctrl_ext = 10,
    covariates = baseline_covariates(
      names = c("age", "score"),
      means_int = c(59, 5),
      covariance_int = covariance_matrix(diag = c(5, 1)),
    )
  )
  data_sim <- create_data_simulation(
    baseline = baseline,
    event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 50)
  )

  enrollment_1 <- enrollment_constant(rate = c(3, 8, 9), for_time = c(2, 3, 4))
  enrollment_2 <- enrollment_constant(rate = c(5, 6, 5), for_time = c(2, 2, 5))
  result <- set_enrollment(data_sim, internal = enrollment_1, external = enrollment_2)
  expect_class(result@enrollment_external, "DataSimEnrollment")
  expect_class(result@enrollment_internal, "DataSimEnrollment")
  expect_equal(result@enrollment_internal@label, "Enrolling patients per time at rates: 3, 3, 8, 8, 8, 9, 9, 9, 9")
  expect_equal(result@enrollment_external@label, "Enrolling patients per time at rates: 5, 5, 6, 6, 5, 5, 5, 5, 5")
})

# cut_off_none -----
test_that("cut_off_none works to create the object", {
  result <- cut_off_none()
  expect_class(result, "DataSimCutOff")
  test_data <- data.frame(id = 1:3, eventtime = 1:3, enrollment = 1:3, status = c(1, 0, 1))
  expect_equal(result@fun(test_data), test_data)
})

# cut_off_after_first -----
test_that("cut_off_after_first works as expected", {
  result <- cut_off_after_first(time = 2)
  expect_class(result, "DataSimCutOff")
  test_data <- data.frame(id = 1:3, eventtime = 1:3, enrollment = 1:3, status = c(1, 1, 1))
  cutoff_data <- result@fun(test_data)
  expected_data <- data.frame(
    id = 1:2, eventtime = c(1, 1), enrollment = 1:2, status = c(1, 0)
  )
  expect_equal(cutoff_data, expected_data)
})

# cut_off_after_last ------
test_that("cut_off_after_last works as expected", {
  result <- cut_off_after_last(time = 2)
  expect_class(result, "DataSimCutOff")
  test_data <- data.frame(id = 1:3, eventtime = c(3, 4, 5), enrollment = 1:3, status = c(1, 1, 1))
  cutoff_data <- result@fun(test_data)
  expected_data <- data.frame(
    id = 1:3, eventtime = c(3, 3, 2), enrollment = 1:3, status = c(1, 0, 0)
  )
  expect_equal(cutoff_data, expected_data)
})

# cut_off_after_events ----

# set_cut_off --------

# set_dropout -----

# create_data_simulation ------

# make_one_dataset --------

# generate -----
test_that("generate works on DataSimObjects", {
  set.seed(1000)
  baseline <- create_baseline_object(
    n_trt_int = 50,
    n_ctrl_int = 40,
    n_ctrl_ext = 10,
    covariates = baseline_covariates(
      names = c("age", "score"),
      means_int = c(59, 5),
      covariance_int = covariance_matrix(diag = c(5, 1)),
    ),
    transformations = list(score_high = binary_cutoff("score", int_cutoff = 0.7, ext_cutoff = 0.7))
  )
  data_sim <- create_data_simulation(
    baseline = baseline,
    drift_hr = 1,
    treatment_hr = 2,
    coefficients = c(age = 0.05, score_high = 1.1),
    event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 50)
  )

  result <- generate(data_sim, drift_hr = c(1, 1.5), treatment_hr = c(1.1, 1.3), n = 2)
  expect_class(result, "SimDataList")
  expect_equal(
    result@guide,
    data.frame(
      sim_id = 1:4,
      treatment_hr = rep(c(1.1, 1.3), times = 2),
      drift_hr = rep(c(1.0, 1.5), each = 2),
      n_datasets_per_param = 2
    )
  )
  expect_list(result@data_list[[1]], "matrix")
  expect_equal(lengths(result@data_list), c(2, 2, 2, 2))
  expect_equal(
    colnames(result@data_list[[1]][[1]]),
    c("patid", "age", "score_high", "trt", "ext", "eventtime", "status", "enrollment", "cens")
  )
})

# show ----
test_that("DataSimObject show works as expected", {
  my_baseline <- create_baseline_object(
    n_trt_int = 100,
    n_ctrl_int = 50,
    n_ctrl_ext = 100,
    covariates = baseline_covariates(
      names = c("age", "score"),
      means_int = c(59, 5),
      means_ext = c(60, 5),
      covariance_int = covariance_matrix(diag = c(5, 1)),
      covariance_ext = covariance_matrix(diag = c(5, 1.2), upper_tri = c(0.1))
    ),
    transformations = list(score_high = binary_cutoff("score", int_cutoff = 0.7, ext_cutoff = 0.7))
  )

  object <- create_data_simulation(
    baseline = my_baseline,
    coefficients = c(age = 0.0005, score_high = 1.1),
    event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 50)
  ) %>%
    set_enrollment(
      internal = enrollment_constant(rate = c(25, 10), for_time = c(4, 30)),
      external = enrollment_constant(rate = c(30, 10), for_time = c(4, 30))
    ) %>%
    set_dropout(
      internal_treated = create_event_dist(dist = "weibull", lambdas = 1 / 50, gammas = 1.2),
      internal_control = create_event_dist(dist = "exponential", lambdas = 1 / 55),
      external_control = create_event_dist(dist = "exponential", lambdas = 1 / 40)
    ) %>%
    set_cut_off(
      internal = cut_off_after_events(n = 80),
      external = cut_off_after_first(time = 65)
    )
  expect_snapshot(show(object))
})

# Testing generated data

test_that("Test simulated data has expected coefficients in Cox model", {
  library(survival)
  my_baseline <- create_baseline_object(
    n_trt_int = 800,
    n_ctrl_int = 700,
    n_ctrl_ext = 100,
    covariates = baseline_covariates(
      names = c("age", "score"),
      means_int = c(59, 5),
      covariance_int = covariance_matrix(diag = c(5, 1)),
    ),
    transformations = list(score_high = binary_cutoff("score", int_cutoff = 0.7, ext_cutoff = 0.7))
  )
  set.seed(10000)
  data <- generate(
    create_data_simulation(
      baseline = my_baseline,
      drift_hr = 1,
      treatment_hr = 2,
      coefficients = c(age = 0.05, score_high = 1.1),
      event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 50)
    )
  )
  fit <- coxph(Surv(eventtime, status) ~ trt + age + score_high, as.data.frame(data@data_list[[1]][[1]]))
  expected_coefs <- c(trt = log(2), age = 0.05, score_high = 1.1)
  expect_equal(fit$coefficients, expected_coefs, tolerance = 0.05)
})

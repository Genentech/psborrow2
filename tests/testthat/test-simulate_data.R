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

# null_event_dist ----
test_that("null_event_dist works as expected", {
  result <- null_event_dist()
  expect_class(result, "DataSimEvent")
  expect_list(result@params, len = 0)
  expect_equal(result@label, "No distribution specified")
})

# DataSimEnrollment -------
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

# create_data_sim_enrollment ----

# set_enrollment -------


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
    event_dist = event_dist(dist = "exponential", lambdas = 1 / 50)
  ) %>%
    set_enrollment(
      internal = enrollment_constant(rate = c(25, 10), for_time = c(4, 30)),
      external = enrollment_constant(rate = c(30, 10), for_time = c(4, 30))
    ) %>%
    set_dropout(
      internal_treated = event_dist(dist = "weibull", lambdas = 1 / 50, gammas = 1.2),
      internal_control = event_dist(dist = "exponential", lambdas = 1 / 55),
      external_control = event_dist(dist = "exponential", lambdas = 1 / 40)
    ) %>%
    set_cut_off(
      internal = cut_off_after_events(n = 80),
      external = cut_off_after_first(time = 65)
    )
  expect_snapshot(show(object))
})

# Testing generated data

test_that("Test simulated data gives expected result with coxph", {

})

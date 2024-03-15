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

# set_enrollment -------

# cut_off_none -----

# cut_off_after_first -----

# cut_off_after_last ------

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

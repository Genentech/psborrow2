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
test_that("cut_off_after_events works as expected", {
  result <- cut_off_after_events(n = 2)
  expect_class(result, "DataSimCutOff")
  test_data <- data.frame(
    id = 1:8,
    status = rep(c(1, 0), length.out = 8),
    eventtime = rep(1:4, each = 2),
    enrollment = rep(2:5, length.out = 8)
  )

  cutoff_data <- result@fun(test_data)

  # for n = 2, 2nd event is at 5 time units: pts 1 and 5
  expected_data <- data.frame(
    id = c(1, 2, 3, 5, 6, 7), # Lose pts who enroll on/after 5 time units
    status = c(1, 0, 0, 1, 0, 0), # Reassign pts who have events > 5 time units (enroll+event)
    eventtime = c(1, 1, 1, 3, 2, 1), # Max follow-up is 5 units,
    enrollment = rep(c(2, 3, 4), length.out = 6)
  )

  expect_equal(cutoff_data, expected_data, ignore_attr = TRUE)
})

test_that("cut_off_after_events works with limit greater than number of observed events", {
  result <- cut_off_after_events(n = 20)
  expect_class(result, "DataSimCutOff")
  test_data <- data.frame(
    id = 1:8,
    status = rep(c(1, 0), length.out = 8),
    eventtime = rep(1:4, each = 2),
    enrollment = rep(2:5, length.out = 8)
  )

  cutoff_data <- result@fun(test_data)

  # expect no observations to be changed
  expect_equal(cutoff_data, test_data)
})

# set_cut_off --------
test_that("set_cut_off works", {
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
  cutoff_1 <- cut_off_after_events(n = 50)
  cutoff_2 <- cut_off_none()
  result <- set_cut_off(data_sim, internal = cutoff_1, external = cutoff_2)
  expect_class(result, "DataSimObject")
  expect_class(result@cut_off_external, "DataSimCutOff")
  expect_class(result@cut_off_internal, "DataSimCutOff")
  expect_equal(cutoff_1, result@cut_off_internal)
  expect_equal(cutoff_2, result@cut_off_external)
})

# set_dropout -----
test_that("set_dropout works", {
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

  dropout_1 <- create_event_dist(dist = "exponential", lambdas = 1 / 50)
  dropout_2 <- create_event_dist(dist = "exponential", lambdas = c(0.002, 0.004), pmix = 0.4, mixture = TRUE)
  dropout_3 <- create_event_dist(dist = "weibull", lambdas = 1 / 50, gammas = 1.1)
  result <- set_dropout(data_sim,
    internal_treated = dropout_1,
    internal_control = dropout_2,
    external_control = dropout_3
  )
  expect_class(result, "DataSimObject")
  expect_equal(result@dropout_internal_treated, dropout_1)
  expect_equal(result@dropout_internal_control, dropout_2)
  expect_equal(result@dropout_external_control, dropout_3)
})

# create_data_simulation ------

test_that("create_data_simulation works as expected", {
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

  expect_class(data_sim, "DataSimObject")
})

test_that("create_data_simulation works catches misspecified parameters", {
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

  # bad coefficient name
  expect_error(
    create_data_simulation(
      baseline = baseline,
      drift_hr = 1,
      treatment_hr = 2,
      coefficients = c(AGE = 0.05, score_high = 1.1),
      event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 50)
    ),
    "Unknown coefficient"
  )

  # bad drift_hr
  expect_error(
    create_data_simulation(
      baseline = baseline,
      drift_hr = "no drift",
      treatment_hr = 2,
      coefficients = c(age = 0.05, score_high = 1.1),
      event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 50)
    ),
    "Must be of type"
  )

  # bad treatment_hr
  expect_error(
    create_data_simulation(
      baseline = baseline,
      drift_hr = 1,
      treatment_hr = NA_real_,
      coefficients = c(age = 0.05, score_high = 1.1),
      event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 50)
    ),
    "missing"
  )
})


test_that("create_data_simulation works with fixed data", {
  historical_trial_data <- data.frame(
    age = rnorm(40, 60, 5),
    score_high = rbinom(40, 1, 0.7),
    trt = 0,
    eventtime = rexp(40, 1 / 50),
    status = 1,
    enrollment = 1 # enrollment is specified here but not used in clinical cut off
  )

  my_internal_baseline <- create_baseline_object(
    n_trt_int = 100,
    n_ctrl_int = 50,
    n_ctrl_ext = 0,
    covariates = baseline_covariates(
      names = c("age", "score"),
      means_int = c(55, 5),
      means_ext = c(60, 5),
      covariance_int = covariance_matrix(diag = c(5, 1)),
      covariance_ext = covariance_matrix(diag = c(5, 1.2), upper_tri = c(0.1))
    ),
    transformations = list(score_high = binary_cutoff("score", int_cutoff = 0.7, ext_cutoff = 0.7))
  )

  my_data_sim_setup_with_fixed <- create_data_simulation(
    baseline = my_internal_baseline,
    coefficients = c(age = 0.001, score_high = 1.1),
    event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 50),
    fixed_external_data = historical_trial_data
  ) %>%
    set_enrollment(
      internal = enrollment_constant(rate = c(25, 10), for_time = c(4, 30)),
      external = enrollment_constant(rate = c(30, 10), for_time = c(4, 30))
    ) %>%
    set_dropout(
      internal_treated = create_event_dist(dist = "exponential", lambdas = 1 / 50),
      internal_control = create_event_dist(dist = "exponential", lambdas = 1 / 55),
      external_control = create_event_dist(dist = "exponential", lambdas = 1 / 40)
    ) %>%
    set_cut_off(
      internal = cut_off_after_first(time = 60),
      external = cut_off_after_events(n = 100)
    )
  expect_class(my_data_sim_setup_with_fixed, "DataSimObject")

  expected_historical <- cbind(historical_trial_data, ext = 1)
  expect_equal(
    my_data_sim_setup_with_fixed@fixed_external_data@data,
    expected_historical
  )

  # same as original data
  set.seed(100)
  generated_data <- generate(my_data_sim_setup_with_fixed, n = 1, treatment_hr = 1, drift_hr = 1)
  fixed_obs <- as.data.frame(generated_data@data_list[[1]][[1]][151:190, ])
  expect_equal(fixed_obs$age, historical_trial_data$age)
  expect_equal(fixed_obs$eventtime, historical_trial_data$eventtime)

  # Drift is not used warning
  expect_warning(
    generate(my_data_sim_setup_with_fixed, n = 1, treatment_hr = 1, drift_hr = 1.2),
    "Drift parameter is not applied to fixed external data"
  )
})

# make_one_dataset --------

test_that("make_one_dataset works as expected", {
  baseline <- create_baseline_object(
    n_trt_int = 30,
    n_ctrl_int = 20,
    n_ctrl_ext = 30,
    covariates = baseline_covariates(
      names = c("age", "score"),
      means_int = c(40, 5),
      means_ext = c(50, 5),
      covariance_int = covariance_matrix(diag = c(5, 1)),
      covariance_ext = covariance_matrix(diag = c(5, 1.2), upper_tri = c(0.1))
    ),
    transformations = list(score_high = binary_cutoff("score", int_cutoff = 0.7, ext_cutoff = 0.7))
  )

  baseline_data <- generate(baseline)[[1]]
  result <- make_one_dataset(
    baseline = baseline_data,
    betas = c(age = 0.001, score_high = 1.1, trt = log(1.5), ext = log(1.1)),
    enrollment = enrollment_constant(5, 20),
    event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 20),
    dropout = create_event_dist(dist = "exponential", lambdas = 1 / 50)
  )
  expect_data_frame(result, nrows = 30)
  expect_equal(colnames(result), c("patid", "age", "score_high", "trt", "ext", "eventtime", "status", "enrollment"))
})

test_that("make_one_dataset works catches bad conditions", {
  baseline <- create_baseline_object(
    n_trt_int = 30,
    n_ctrl_int = 20,
    n_ctrl_ext = 30,
    covariates = baseline_covariates(
      names = c("age", "score"),
      means_int = c(40, 5),
      means_ext = c(50, 5),
      covariance_int = covariance_matrix(diag = c(5, 1)),
      covariance_ext = covariance_matrix(diag = c(5, 1.2), upper_tri = c(0.1))
    ),
    transformations = list(score_high = binary_cutoff("score", int_cutoff = 0.7, ext_cutoff = 0.7))
  )

  baseline_data <- generate(baseline)[[1]]

  bad_event_dist <- create_event_dist(loghazard = function(t, x, betas, ...) stop("bad log hazard definition"))

  expect_error(
    expect_output(
      make_one_dataset(
        baseline = baseline_data,
        betas = c(age = 0.001, score_high = 1.1, trt = log(1.5), ext = log(1.1)),
        enrollment = enrollment_constant(5, 20),
        event_dist = bad_event_dist,
        dropout = create_event_dist(dist = "exponential", lambdas = 1 / 50)
      ),
      "Error caught when generating survival times"
    ),
    "bad log hazard"
  )

  expect_error(
    expect_output(
      make_one_dataset(
        baseline = baseline_data,
        betas = c(age = 0.001, score_high = 1.1, trt = log(1.5), ext = log(1.1)),
        enrollment = enrollment_constant(5, 20),
        event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 50),
        dropout = bad_event_dist
      ),
      "Error caught when generating drop out times"
    ),
    "bad log hazard"
  )

  expect_warning(
    make_one_dataset(
      baseline = baseline_data,
      betas = c(age = 0.001, score_high = 1.1, trt = log(1.5), ext = log(1.1)),
      enrollment = custom_enrollment(function(n) sample(-10:1, size = n, replace = TRUE), "negative times"),
      event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 50),
      dropout = create_event_dist(dist = "exponential", lambdas = 1 / 500)
    ),
    "Negative enrollment times were generated"
  )
})


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

test_that("Test simulated data has expected coefficients in models", {
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

  survreg_fit <- survreg(Surv(eventtime, status) ~ trt + age + score_high,
    data = as.data.frame(data@data_list[[1]][[1]]), dist = "exponential"
  )
  expected_survreg_coefs <- c(trt = -log(2), age = -0.05, score_high = -1.1)
  expect_equal(survreg_fit$coefficients[2:4], expected_survreg_coefs, tolerance = 0.025)
})

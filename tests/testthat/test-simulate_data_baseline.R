test_that("covariance_matrix works", {
  result <- covariance_matrix(c(1:4), c(1, 0, 0, 0, 0, 0))
  expected <- matrix(
    c(
      1, 1, 0, 0,
      1, 2, 0, 0,
      0, 0, 3, 0,
      0, 0, 0, 4
    ),
    nrow = 4
  )

  expect_equal(result, expected)
})

test_that("covariance_matrix finds positive definite matrices", {
  expect_warning(
    result <- covariance_matrix(c(1:4), c(1, 4, -2, 1, 0, 0)),
    "Finding nearest positive definite matrix"
  )
  expected <- matrix(
    c(
      1, -0.173717259157214, 1.55477286166658, 0.606677145265074,
      -0.173717259157214, 2, -1.25191988052734, 0.120331114550728,
      1.55477286166658, -1.25191988052734, 3, 0.250687992911699,
      0.606677145265074, 0.120331114550728, 0.250687992911699, 4
    ),
    nrow = 4
  )

  expect_equal(result, expected)
})


test_that("covariance_matrix fails with incompatible arguments", {
  expect_error(covariance_matrix(c(1:3), c(0, 0)))
  expect_error(covariance_matrix(c("a", "b", "c")))
})


test_that("show CorrelatedCovariates works for narrow matrices", {
  with_age <- create_baseline_object(
    100, 50, 100,
    covariates = baseline_covariates(
      names = "age", means_int = 55,
      covariance_int = covariance_matrix(5)
    )
  )
  expect_snapshot(show(with_age))
})


test_that("show CorrelatedCovariates works for wide matrices", {
  with_age <- create_baseline_object(
    100, 50, 100,
    covariates = baseline_covariates(
      names = "age_at_baseline", means_int = 55,
      covariance_int = covariance_matrix(5)
    )
  )
  expect_snapshot(show(with_age))
})


test_that("show CorrelatedCovariates works for very wide matrices", {
  with_age <- create_baseline_object(
    100, 50, 100,
    covariates = baseline_covariates(
      names = "number_of_years_since_the_date_of_birth_at_baseline_date", means_int = 55,
      covariance_int = covariance_matrix(5)
    )
  )
  expect_snapshot(show(with_age))
})


test_that("adding transformation works when transformations exist", {
  baseline <- create_baseline_object(
    100, 50, 100,
    covariates = baseline_covariates(
      names = "age", means_int = 55,
      covariance_int = covariance_matrix(5)
    ),
    transformations = list(
      age_sq = function(data) data$age^2
    )
  )
  result <- set_transformations(baseline, age_scaled = function(data) scale(data$age))
  expect_list(result@transformations, types = "function", len = 2)
  expect_names(names(result@transformations), identical.to = c("age_sq", "age_scaled"))
  expect_equal(result@transformations[["age_scaled"]](data.frame(age = 1:10)), scale(1:10))
})

test_that("adding transformation works when transformations exist and overwrite=TRUE", {
  baseline <- create_baseline_object(
    100, 50, 100,
    covariates = baseline_covariates(
      names = "age", means_int = 55,
      covariance_int = covariance_matrix(5)
    ),
    transformations = list(
      age_sq = function(data) data$age^2
    )
  )
  result <- set_transformations(baseline, age_scaled = function(data) scale(data$age), overwrite = TRUE)
  expect_list(result@transformations, types = "function", len = 1)
  expect_names(names(result@transformations), identical.to = c("age_scaled"))
})

test_that("adding transformation works with no transformations exist", {
  baseline <- create_baseline_object(
    100, 50, 100,
    covariates = baseline_covariates(
      names = "age", means_int = 55,
      covariance_int = covariance_matrix(5)
    )
  )
  result <- set_transformations(baseline, age_months = function(data) data$age * 12)
  expect_list(result@transformations, types = "function", len = 1)
  expect_names(names(result@transformations), identical.to = c("age_months"))
})

test_that("adding transformation fails for bad input", {
  baseline <- create_baseline_object(
    100, 50, 100,
    covariates = baseline_covariates(
      names = "age", means_int = 55,
      covariance_int = covariance_matrix(5)
    )
  )
  expect_error(set_transformations(baseline, age_months = 1:200), "function")
  expect_warning(
    set_transformations(
      baseline,
      age_months = function(data) data$age * 12,
      age_months = function(data) data$age * 12
    ),
    "Multiple"
  )
  expect_error(set_transformations(baseline, function(data) -data$age), "names")
})

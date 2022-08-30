test_that("h_glue works as expected", {
  number <- 3.141592653587
  string <- "abc"
  expect_equal(h_glue("pi is {{number}}"), "pi is 3.141592653587")
  expect_equal(h_glue("{{1+2}}", "{{string}}"), "3abc")
  expect_equal(h_glue("{string}"), "{string}")
})

test_that("rename_draws_covariates works as expected", {
  analysis_object <- psborrow2:::.analysis_obj(
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

  draws_object <- structure(
    c(
      -386.28, -386.722, -387.243, -386.91, -388.966, -390.036,
      -387.429, -389.043, -384.904, -384.904, 2.58428, 2.26318, 2.25025,
      2.12663, 1.93748, 2.57816, 1.86276, 1.69333, 2.06932, 2.06932,
      15.1172, 7.06106, 4.71234, 6.56951, 6.37716, 104.612, 46.7026,
      22.338, 1199.03, 1199.03, 1.11312, 0.870231, 0.85078, 0.788673,
      1.38307, 1.00293, 1.08662, 1.07846, 1.15029, 1.15029, 1.56568,
      1.3782, 1.38935, 1.50072, 1.60854, 1.25794, 1.3719, 1.25818,
      1.16015, 1.16015, -0.720938, -0.935607, -0.970029, -0.768617,
      -0.732576, -0.349002, -0.862976, -0.518073, -0.729685, -0.729685,
      -1.48761, -1.04348, -0.996602, -1.12434, -1.39049, -1.46425,
      -0.956735, -1.26619, -1.05455, -1.05455, 13.2537, 9.61362, 9.49011,
      8.38656, 6.94126, 13.1729, 6.44152, 5.43756, 7.91942, 7.91942
    ),
    .Dim = c(10L, 1L, 8L),
    .Dimnames = list(
      iteration = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
      chain = "1",
      variable = c("lp__", "beta_trt", "tau", "alpha[1]", "alpha[2]", "beta[1]", "beta[2]", "OR_trt")
    ),
    class = c("draws_array", "draws", "array")
  )

  result <- rename_draws_covariates(draws_object, analysis_object)
  expect_class(result, "draws")
  expect_equal(
    dimnames(result)$variable,
    c("lp__", "beta_trt", "tau", "alpha[1]", "alpha[2]", "b_cov1", "b_cov2", "OR_trt")
  )
})

test_that("variable_dictionary works as expected for logistic and BDB", {
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
  result <- variable_dictionary(object)
  expect_equal(
    result,
    data.frame(
      Stan_variable = c("tau", "alpha[1]", "alpha[2]", "beta[1]", "beta[2]", "beta_trt", "exp_trt"),
      Description = c(
        "commensurability parameter", "intercept, internal", "intercept, external",
        "cov1", "cov2", "treatment log OR", "treatment OR"
      )
    )
  )
})

test_that("variable_dictionary works as expected for exponential and no borrowing", {
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
  result <- variable_dictionary(object)
  expect_equal(
    result,
    data.frame(
      Stan_variable = c("alpha", "beta_trt", "exp_trt"),
      Description = c("baseline log hazard rate", "treatment log HR", "treatment HR")
    )
  )
})

test_that("variable_dictionary includes shape parameter for Weibull PH", {
  object <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    outcome = weib_ph_surv_dist("time", "cnsr", normal_prior(0, 1000)),
    borrowing = borrowing_details(
      "Full borrowing",
      normal_prior(0, 100),
      "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 1000))
  )
  result <- variable_dictionary(object)
  expect_equal(
    result,
    data.frame(
      Stan_variable = c("alpha", "beta_trt", "exp_trt", "shape_weibull"),
      Description = c("baseline log hazard rate", "treatment log HR", "treatment HR", "Weibull shape parameter")
    )
  )
})

test_that("Half Cauchy priors are rendering correctly", {
  # Make normal prior
  object <- half_cauchy_prior(mu = 2, sigma = 3)

  # Expect correct class
  expect_class(object, "HalfCauchyPrior")
  expect_equal(object@mu, 2L)
  expect_equal(object@sigma, 3L)
  expect_equal(psborrow2:::h_glue(object@constraint), "<lower=2>")

  # Expect N inputs correct
  expect_equal(NROW(slotNames(object)) - 3, object@n_param)

  # Errors
  expect_error(half_cauchy_prior(mu = 2, sigma = -1),
    regexp = "invalid class .HalfCauchyPrior. object: sigma must be >0"
  )
})

test_that("show works for HalfCauchyPrior", {
  expect_snapshot_output(show(half_cauchy_prior(0, 0.8)))
})

test_that("plot works for HalfCauchyPrior", {
  vdiffr::expect_doppelganger(
    "hafl_cauchy_prior_plot",
    plot(half_cauchy_prior(0, 0.8))
  )
})


test_that("the STAN code is correctly generated for Half Cauchy", {
  stan_model_string <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = exp_surv_dist("time", "cnsr", normal_prior(0, 100000)),
    borrowing = borrowing_details("Full borrowing",
                                  ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", half_cauchy_prior(0,20))
  )
})

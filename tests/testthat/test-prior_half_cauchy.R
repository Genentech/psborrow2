test_that("Half Cauchy priors are rendering correctly", {
  # Make normal prior
  object <- prior_half_cauchy(mu = 2, sigma = 3)

  # Expect correct class
  expect_class(object, "PriorHalfCauchy")
  expect_equal(object@mu, 2L)
  expect_equal(object@sigma, 3L)
  expect_equal(psborrow2:::h_glue(object@constraint), "<lower=2>")

  # Expect N inputs correct
  expect_equal(NROW(slotNames(object)) - 3, object@n_param)

  # Errors
  expect_error(prior_half_cauchy(mu = 2, sigma = -1),
    regexp = "invalid class .PriorHalfCauchy. object: sigma must be >0"
  )
})

test_that("show works for PriorHalfCauchy", {
  expect_snapshot_output(show(prior_half_cauchy(0, 0.8)))
})

test_that("plot works for PriorHalfCauchy", {
  vdiffr::expect_doppelganger(
    "prior_half_cauchy_plot",
    plot(prior_half_cauchy(0, 0.8))
  )
})


test_that("constraints work for PriorHalfCauchy", {
  expect_equal(eval_constraints(prior_half_cauchy(2, 5)), "<lower=2>")
  expect_equal(eval_constraints(prior_half_cauchy(4, 5)), "<lower=4>")
  expect_equal(eval_constraints(prior_half_cauchy(200, 10000)), "<lower=200>")
})

test_that("half_cauchy_prior() throws error", {
  expect_error(half_cauchy_prior(),
    regexp = "deprecated"
  )

  expect_error(half_cauchy_prior(0, 0.8),
    regexp = "deprecated"
  )
})

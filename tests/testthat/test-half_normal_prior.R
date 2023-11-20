test_that("Half Normal priors are rendering correctly", {
  # Make normal prior
  object <- half_prior_normal(mu = 2, sigma = 3)

  # Expect correct class
  expect_class(object, "HalfNormalPrior")
  expect_equal(object@mu, 2L)
  expect_equal(object@sigma, 3L)
  expect_equal(psborrow2:::h_glue(object@constraint), "<lower=2>")

  # Expect N inputs correct
  expect_equal(NROW(slotNames(object)) - 3, object@n_param)

  # Errors
  expect_error(half_prior_normal(mu = 2, sigma = -1),
    regexp = "invalid class .HalfNormalPrior. object: sigma must be >0"
  )
})

test_that("show works for HalfNormalPrior", {
  expect_snapshot_output(show(half_prior_normal(0, 0.8)))
})

test_that("plot works for HalfNormalPrior", {
  vdiffr::expect_doppelganger(
    "half_prior_normal_plot",
    plot(half_prior_normal(0, 0.8))
  )
})


test_that("constraints work for HalfNormalPrior", {
  expect_equal(eval_constraints(half_prior_normal(2, 5)), "<lower=2>")
  expect_equal(eval_constraints(half_prior_normal(4, 5)), "<lower=4>")
  expect_equal(eval_constraints(half_prior_normal(200, 10000)), "<lower=200>")
})

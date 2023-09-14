test_that("Uniform priors are rendering correctly", {
  # Make uniform prior
  prior <- uniform_prior(alpha = 20, beta = 300)

  # Expect correct class
  expect_class(prior, "UniformPrior")
  expect_equal(prior@alpha, 20)
  expect_equal(prior@beta, 300)
  expect_equal(eval_constraints(prior), "<lower=20,upper=300>")

  # Expect N inputs correct
  expect_equal(NROW(slotNames(prior)) - 3, prior@n_param)

  # Errors
  expect_error(uniform_prior(alpha = -1, beta = -2),
    regexp = "invalid class .UniformPrior. object: beta must be > alpha"
  )
})

test_that("show works for UniformPrior", {
  expect_snapshot_output(show(uniform_prior(0, 3)))
})


test_that("plot works for UniformPrior", {
  vdiffr::expect_doppelganger(
    "uniform_prior_plot",
    plot(uniform_prior(0, 3))
  )
})

test_that("constraints work for UniformPrior", {
  expect_equal(eval_constraints(uniform_prior(0, 3)), "<lower=0,upper=3>")
})

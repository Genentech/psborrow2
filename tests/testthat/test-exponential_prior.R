test_that("Exponential priors are rendering correctly", {
  # Make exponential prior
  prior <- exponential_prior(beta = 3L)

  # Expect correct class
  expect_class(prior, "ExponentialPrior")
  expect_equal(prior@beta, 3L)

  # Expect N inputs correct
  expect_equal(NROW(slotNames(prior)) - 3, prior@n_param)

  # Errors
  expect_error(exponential_prior(beta = -1L),
    regexp = "invalid class .ExponentialPrior. object: beta must be >0"
  )
})

test_that("summary works for ExponentialPrior", {
  expect_snapshot_output(summary(exponential_prior(4)))
})

test_that("plot works for ExponentialPrior", {
  vdiffr::expect_doppelganger(
    "gamma_prior_plot",
    plot(exponential_prior(4))
  )
})

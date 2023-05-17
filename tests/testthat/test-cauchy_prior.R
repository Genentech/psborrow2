test_that("Cauchy priors are rendering correctly", {
  # Make normal prior
  prior <- cauchy_prior(mu = 2, sigma = 3)

  # Expect correct class
  expect_class(prior, "CauchyPrior")
  expect_equal(prior@mu, 2L)
  expect_equal(prior@sigma, 3L)

  # Expect N inputs correct
  expect_equal(NROW(slotNames(prior)) - 3, prior@n_param)

  # Errors
  expect_error(cauchy_prior(mu = 2, sigma = -1),
    regexp = "invalid class .CauchyPrior. object: sigma must be >0"
  )
})

test_that("show works for CauchyPrior", {
  expect_snapshot_output(show(cauchy_prior(0, 0.8)))
})

test_that("plot works for CauchyPrior", {
  vdiffr::expect_doppelganger(
    "cauchy_prior_plot",
    plot(cauchy_prior(0, 0.8))
  )
})

test_that("constraints work for CauchyPrior", {
  expect_equal(eval_constraints(cauchy_prior(0, 0.8)), "")
})




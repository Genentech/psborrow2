test_that("Poisson priors are rendering correctly", {
  # Make poisson prior
  prior <- poisson_prior(lambda = 0.2)

  # Expect correct class
  expect_class(prior, "PoissonPrior")
  expect_equal(prior@lambda, 0.2)

  # Expect N inputs correct
  expect_equal(NROW(slotNames(prior)) - 3, prior@n_param)

  # Errors
  expect_error(poisson_prior(-1),
    regexp = "invalid class .PoissonPrior. object: lambda must be >0"
  )
})

test_that("show works for PoissonPrior", {
  expect_snapshot_output(show(poisson_prior(3)))
})

test_that("plot works for PoissonPrior", {
  vdiffr::expect_doppelganger(
    "poisson_prior_plot",
    plot(poisson_prior(3))
  )
})

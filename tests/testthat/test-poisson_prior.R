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

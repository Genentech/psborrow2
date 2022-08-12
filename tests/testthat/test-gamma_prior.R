test_that("Gamma priors are rendering correctly", {
  # Make gamma prior
  prior <- gamma_prior(alpha = 2, beta = 3)

  # Expect correct class
  expect_class(prior, "GammaPrior")
  expect_equal(prior@alpha, 2L)
  expect_equal(prior@beta, 3L)

  # Expect N inputs correct
  expect_equal(NROW(slotNames(prior)) - 3, prior@n_param)

  # Errors
  expect_error(gamma_prior(alpha = -1, beta = -2),
    regexp = "invalid class .GammaPrior. object: Both alpha and beta must be >= 0"
  )
  expect_error(gamma_prior(alpha = 1, beta = -2),
    regexp = "invalid class .GammaPrior. object: Both alpha and beta must be >= 0"
  )
})

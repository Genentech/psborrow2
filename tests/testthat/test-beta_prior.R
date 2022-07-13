test_that("Beta priors are rendering correctly", {
   # Make beta prior
   prior <- beta_prior(alpha = 2, beta = 3)

   # Expect correct class
   expect_class(prior, "BetaPrior")
   expect_equal(prior@alpha, 2L)
   expect_equal(prior@beta, 3L)

   # Expect N inputs correct
   expect_equal(NROW(slotNames(prior)) - 2, prior@n_param)

   # Errors
   expect_error(beta_prior(alpha = -1, beta = -2),
      regexp = "invalid class .BetaPrior. object: Both alpha and beta must be >= 0"
   )
})

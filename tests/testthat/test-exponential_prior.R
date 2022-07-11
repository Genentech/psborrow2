test_that("Exponential priors are rendering correctly", {

   # Make exponential prior
   prior <- exponential_prior(beta = 3L)

   # Expect correct class
   expect_class(prior, "ExponentialPrior")
   expect_equal(prior@beta, 3L)

   # Expect N inputs correct
   expect_equal(NROW(slotNames(prior))-2, prior@n_param)

   # Errors
   expect_error(exponential_prior(beta = -1L),
                regexp = "invalid class \"ExponentialPrior\" object: beta must be >0")
})

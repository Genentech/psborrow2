test_that("Uniform priors are rendering correctly", {

   # Make uniform prior
   prior <- uniform_prior(alpha = 20, beta = 300)

   # Expect correct class
   expect_class(prior, "UniformPrior")
   expect_equal(prior@alpha, 20)
   expect_equal(prior@beta, 300)

   # Expect N inputs correct
   expect_equal(NROW(slotNames(prior))-2, prior@n_param)

   # Errors
   expect_error(uniform_prior(alpha = -1, beta = -2),
                regexp = "invalid class “UniformPrior” object")
})

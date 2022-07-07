test_that("Bernoulli priors are rendering correctly", {

   # Make bernoulli prior
   prior <- bernoulli_prior(theta = 0.99)

   # Expect correct class
   expect_class(prior, "BernoulliPrior")
   expect_equal(prior@theta, .99)

   # Expect N inputs correct
   expect_equal(NROW(slotNames(prior))-2, prior@n_param)

   # Errors
   expect_error(bernoulli_prior(1.2),
                regexp = "invalid class “BernoulliPrior” object")
})

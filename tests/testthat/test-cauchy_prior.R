test_that("Cauchy priors are rendering correctly", {

   # Make normal prior
   prior <- cauchy_prior(mu = 2, sigma = 3)

   # Expect correct class
   expect_class(prior, "CauchyPrior")
   expect_equal(prior@mu, 2L)
   expect_equal(prior@sigma, 3L)

   # Errors
   expect_error(cauchy_prior(mu = 2, sigma = -1),
                regexp = "invalid class “CauchyPrior” object: sigma must be >0")
})

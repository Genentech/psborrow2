test_that("Normal priors are rendering correctly", {

   # Make normal prior
   prior <- normal_prior(mu = 2, sigma = 3)

   # Expect correct class
   expect_class(prior, "NormalPrior")
   expect_equal(prior@mu, 2L)
   expect_equal(prior@sigma, 3L)

   # Errors
   expect_error(normal_prior(mu = 2, sigma = -1),
                regexp = 'invalid class “NormalPrior” object: sigma must be >0')
})

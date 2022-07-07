test_that("Normal priors are rendering correctly", {

   # Make normal prior
   prior <- normal_prior(mu = 2, sigma = 3)

   # Expect correct class
   expect_class(prior, "NormalPrior")
   expect_equal(prior@mu, 2L)
   expect_equal(prior@sigma, 3L)

   # Expect N inputs correct
   expect_equal(NROW(slotNames(prior))-2, prior@n_param)

   # Errors
   expect_error(normal_prior(mu = 2, sigma = -1),
                regexp = "invalid class “NormalPrior” object: sigma must be >0")
})

test_that("Exponential survival distributions are rendering correctly", {
   # Make exponential survival distribution
   surv_dist <- exp_surv_dist()

   # Expect correct class
   expect_class(surv_dist, "ExponentialSurvDist")
   expect_equal(surv_dist@n_param, 0L)
   expect_equal(surv_dist@param_priors, list())

   # Errors
   expect_error(exp_surv_dist(2),
      regexp = "unused argument"
   )
})

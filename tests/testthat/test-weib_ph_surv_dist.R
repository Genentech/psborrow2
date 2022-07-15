test_that("Weibull survival distributions are rendering correctly", {
   # Make Weibull survival distribution
   surv_dist <- weib_ph_surv_dist()

   # Expect correct class
   expect_class(surv_dist, "WeibullPHSurvDist")
   expect_equal(surv_dist@n_param, 1L)
   expect_true(is(surv_dist@param_priors$shape_weibull, "Prior"))

   # Errors
   expect_error(weib_ph_surv_dist(3),
      regexp = "shape_weibull must be of class 'Prior'"
   )
})

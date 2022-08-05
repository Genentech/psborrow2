test_that("set_outcome works as expected", {
   # Create outcome objects
   exp_tte <- exp_surv_dist()
   weib_tte <- weib_ph_surv_dist(shape_prior = normal_prior(0, 1000))

   # Function calls
   exp_tte_o <- set_outcome(exp_tte, time_var = "time", cens_var = "cens")
   weib_tte_o <- set_outcome(weib_tte, time_var = "time", cens_var = "cens")

   # Check classes
   expect_class(exp_tte_o, "ExponentialSurvDist")
   expect_class(weib_tte_o, "WeibullPHSurvDist")

   # See that the columns were added
   expect_equal(exp_tte@cens_var, character(0))
   expect_equal(weib_tte@time_var, character(0))
   expect_equal(exp_tte_o@cens_var, "cens")
   expect_equal(weib_tte_o@time_var, "time")

   # Errors
   expect_error(set_outcome(outcome_obj = " outcome_obj must be a time to event or binary outcome object"))
})

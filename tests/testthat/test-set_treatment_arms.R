test_that("set_treatment_arms works as expected", {

   # Create objects
   sta <- set_treatment_arms(
      ext_flag_col = 'ext',
      trt_flag_col = 'trt',
      trt_log_hazard_ratio_prior = normal_prior(0, 10000),
      ext_log_hazard_rate_prior = normal_prior(0, 10000)
      )

   # Check classes
   expect_class(sta, "Treatment")

   # See that the columns were added
   expect_equal(sta@trt_log_hazard_ratio_prior, normal_prior(0, 10000))
   expect_equal(sta@trt_flag_col, "trt")

   # Errors
   expect_error(set_treatment_arms(ext_flag_col = 1))
   expect_error(set_treatment_arms("ext","trt","hi","there"))

})

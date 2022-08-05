test_that("set_treatment_arms works as expected", {
   # Create objects
   sta <- set_treatment_arms(
      trt_flag_col = "trt",
      trt_prior = normal_prior(0, 10000)
   )

   # Check classes
   expect_class(sta, "Treatment")

   # See that the columns were added
   expect_equal(sta@trt_prior, normal_prior(0, 10000))
   expect_equal(sta@trt_flag_col, "trt")

   # Errors
   expect_error(set_treatment_arms(trt_flag_col = 1))
   expect_error(set_treatment_arms("trt", "hi", "there"))
})

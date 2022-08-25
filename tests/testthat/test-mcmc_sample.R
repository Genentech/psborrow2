test_that("mcmc_sample() default method throws error", {
  df_wrong_input <- data.frame(a = 2:4, b = 3:5)
  expect_error(
    mcmc_sample(df_wrong_input),
    "Objects of type .data.frame. not supported by .mcmc_sample()."
  )
})

skip_if_not(check_cmdstan())

test_that("check_cmdstanr runs as expected", {
  expect_output(
    check_cmdstanr(check_sampling = FALSE),
    "int<lower=0> N;",
    fixed = TRUE
  )

  skip_on_ci()
  skip_on_cran()
  result <- check_cmdstanr(check_sampling = TRUE)
  expect_class(result, "CmdStanMCMC")
})

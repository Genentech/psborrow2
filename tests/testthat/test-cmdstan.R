test_that("check_cmdstanr runs as expected", {
  expect_output(
    result <- check_cmdstanr(),
    "int<lower=0> N;",
    fixed = TRUE
  )
  expect_class(result, "CmdStanMCMC")
})

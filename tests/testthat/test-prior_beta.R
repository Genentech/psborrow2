test_that("Beta priors are rendering correctly", {
  # Make beta prior
  prior <- prior_beta(alpha = 2, beta = 3)

  # Expect correct class
  expect_class(prior, "PriorBeta")
  expect_equal(prior@alpha, 2L)
  expect_equal(prior@beta, 3L)

  # Expect N inputs correct
  expect_equal(NROW(slotNames(prior)) - 3, prior@n_param)

  # Errors
  expect_error(prior_beta(alpha = -1, beta = -2),
    regexp = "invalid class .PriorBeta. object: Both alpha and beta must be >= 0"
  )
})

test_that("show works for PriorBeta", {
  expect_snapshot_output(show(prior_beta(1.5, 0.8)))
})

test_that("plot works for PriorBeta", {
  vdiffr::expect_doppelganger(
    "prior_beta_plot",
    plot(prior_beta(1.5, 0.8))
  )
})

test_that("constraints work for PriorBeta", {
  expect_equal(eval_constraints(prior_beta(1.5, 0.8)), "<lower=0, upper=1>")
})

test_that("beta_prior() throws error", {
  expect_error(beta_prior(),
    regexp = "deprecated"
  )

  expect_error(beta_prior(1.5, 0.8),
    regexp = "deprecated"
  )
})

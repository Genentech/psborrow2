test_that("Gamma priors are rendering correctly", {
  # Make gamma prior
  prior <- prior_gamma(alpha = 2, beta = 3)

  # Expect correct class
  expect_class(prior, "PriorGamma")
  expect_equal(prior@alpha, 2L)
  expect_equal(prior@beta, 3L)

  # Expect N inputs correct
  expect_equal(NROW(slotNames(prior)) - 3, prior@n_param)

  # Errors
  expect_error(prior_gamma(alpha = -1, beta = -2),
    regexp = "invalid class .PriorGamma. object: Both alpha and beta must be >= 0"
  )
  expect_error(prior_gamma(alpha = 1, beta = -2),
    regexp = "invalid class .PriorGamma. object: Both alpha and beta must be >= 0"
  )
})

test_that("show works for PriorGamma", {
  expect_snapshot_output(show(prior_gamma(2, 5)))
})

test_that("plot works for PriorGamma", {
  vdiffr::expect_doppelganger(
    "prior_gamma_plot",
    plot(prior_gamma(2, 5))
  )
})

test_that("constraints work for PriorGamma", {
  expect_equal(eval_constraints(prior_gamma(2, 5)), "<lower=0>")
})

test_that("gamma_prior() throws error", {
  expect_error(gamma_prior(),
    regexp = "deprecated"
  )

  expect_error(gamma_prior(2, 5),
    regexp = "deprecated"
  )
})

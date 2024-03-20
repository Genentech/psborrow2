test_that("Exponential priors are rendering correctly", {
  # Make exponential prior
  prior <- prior_exponential(beta = 3L)

  # Expect correct class
  expect_class(prior, "PriorExponential")
  expect_equal(prior@beta, 3L)

  # Expect N inputs correct
  expect_equal(NROW(slotNames(prior)) - 3, prior@n_param)

  # Errors
  expect_error(prior_exponential(beta = -1L),
    regexp = "invalid class .PriorExponential. object: beta must be >0"
  )
})

test_that("show works for PriorExponential", {
  expect_snapshot_output(show(prior_exponential(4)))
})

test_that("plot works for PriorExponential", {
  vdiffr::expect_doppelganger(
    "prior_gamma_plot",
    plot(prior_exponential(4))
  )
})

test_that("constraints work for PriorExponential", {
  expect_equal(eval_constraints(prior_exponential(4)), "<lower=0>")
})

test_that("exponential_prior() throws error", {
  expect_error(exponential_prior(),
    regexp = "deprecated"
  )

  expect_error(exponential_prior(4),
    regexp = "deprecated"
  )
})

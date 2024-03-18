test_that("Cauchy priors are rendering correctly", {
  # Make normal prior
  prior <- prior_cauchy(mu = 2, sigma = 3)

  # Expect correct class
  expect_class(prior, "PriorCauchy")
  expect_equal(prior@mu, 2L)
  expect_equal(prior@sigma, 3L)

  # Expect N inputs correct
  expect_equal(NROW(slotNames(prior)) - 3, prior@n_param)

  # Errors
  expect_error(prior_cauchy(mu = 2, sigma = -1),
    regexp = "invalid class .PriorCauchy. object: sigma must be >0"
  )
})

test_that("show works for PriorCauchy", {
  expect_snapshot_output(show(prior_cauchy(0, 0.8)))
})

test_that("plot works for PriorCauchy", {
  vdiffr::expect_doppelganger(
    "prior_cauchy_plot",
    plot(prior_cauchy(0, 0.8))
  )
})

test_that("constraints work for PriorCauchy", {
  expect_equal(eval_constraints(prior_cauchy(0, 0.8)), "")
})


test_that("cauchy_prior() throws error", {
  expect_error(cauchy_prior(),
    regexp = "deprecated"
  )

  expect_error(cauchy_prior(0, 0.8),
    regexp = "deprecated"
  )
})

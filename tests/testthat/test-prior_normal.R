test_that("Normal priors are rendering correctly", {
  # Make normal prior
  prior <- prior_normal(mu = 2, sigma = 3)

  # Expect correct class
  expect_class(prior, "PriorNormal")
  expect_equal(prior@mu, 2L)
  expect_equal(prior@sigma, 3L)

  # Expect N inputs correct
  expect_equal(NROW(slotNames(prior)) - 3, prior@n_param)

  # Errors
  expect_error(prior_normal(mu = 2, sigma = -1),
    regexp = "invalid class .PriorNormal. object: sigma must be >0"
  )
})

test_that("show works for PriorNormal", {
  expect_snapshot_output(show(prior_normal(0, 0.8)))
})

test_that("plot works for PriorNormal", {
  vdiffr::expect_doppelganger(
    "prior_normal_plot",
    plot(prior_normal(0, 0.8))
  )
})

test_that("constraints work for PriorNormal", {
  expect_equal(eval_constraints(prior_normal(2, 5)), "")
})


test_that("normal_prior() throws error", {
  expect_error(normal_prior(),
    regexp = "deprecated"
  )

  expect_error(normal_prior(0, 0.8),
    regexp = "deprecated"
  )
})

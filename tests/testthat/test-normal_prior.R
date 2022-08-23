test_that("Normal priors are rendering correctly", {
  # Make normal prior
  prior <- normal_prior(mu = 2, sigma = 3)

  # Expect correct class
  expect_class(prior, "NormalPrior")
  expect_equal(prior@mu, 2L)
  expect_equal(prior@sigma, 3L)

  # Expect N inputs correct
  expect_equal(NROW(slotNames(prior)) - 3, prior@n_param)

  # Errors
  expect_error(normal_prior(mu = 2, sigma = -1),
    regexp = "invalid class .NormalPrior. object: sigma must be >0"
  )
})

test_that("show works for NormalPrior", {
  expect_snapshot_output(show(normal_prior(0, 0.8)))
})

test_that("plot works for NormalPrior", {
  vdiffr::expect_doppelganger(
    "normal_prior_plot",
    plot(normal_prior(0, 0.8))
  )
})

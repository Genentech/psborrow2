test_that("Bernoulli priors are rendering correctly", {
  # Make bernoulli prior
  prior <- bernoulli_prior(theta = 0.99)

  # Expect correct class
  expect_class(prior, "BernoulliPrior")
  expect_equal(prior@theta, .99)

  # Expect N inputs correct
  expect_equal(NROW(slotNames(prior)) - 3, prior@n_param)

  # Errors
  expect_error(bernoulli_prior(1.2),
    regexp = "invalid class .BernoulliPrior. object"
  )
})

test_that("show works for BernoulliPrior", {
  expect_snapshot_output(show(bernoulli_prior(0.7)))
})

test_that("plot works for BernoulliPrior", {
  vdiffr::expect_doppelganger(
    "bernoulli_prior_plot",
    plot(bernoulli_prior(0.7))
  )
})

test_that("get_stan_code works for BernoulliPrior", {
  expect_equal(get_stan_code(bernoulli_prior(0.7)), "bernoulli(0.7)")
})

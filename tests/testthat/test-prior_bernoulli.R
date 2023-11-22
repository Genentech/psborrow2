test_that("Bernoulli priors are rendering correctly", {
  # Make bernoulli prior
  prior <- prior_bernoulli(theta = 0.99)

  # Expect correct class
  expect_class(prior, "PriorBernoulli")
  expect_equal(prior@theta, .99)

  # Expect N inputs correct
  expect_equal(NROW(slotNames(prior)) - 3, prior@n_param)

  # Errors
  expect_error(prior_bernoulli(1.2),
    regexp = "invalid class .PriorBernoulli. object"
  )
})

test_that("show works for PriorBernoulli", {
  expect_snapshot_output(show(prior_bernoulli(0.7)))
})

test_that("plot works for PriorBernoulli", {
  vdiffr::expect_doppelganger(
    "prior_bernoulli_plot",
    plot(prior_bernoulli(0.7))
  )
})

test_that("constraints work for PriorBernoulli", {
  expect_equal(eval_constraints(prior_bernoulli(theta = 0.99)), "<lower=0, upper=1>")
})


test_that("bernoulli_prior() throws error", {
  expect_error(bernoulli_prior(),
    regexp = "deprecated"
  )

  expect_error(bernoulli_prior(theta = 0.2),
    regexp = "deprecated"
  )
})

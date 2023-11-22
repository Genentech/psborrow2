test_that("Poisson priors are rendering correctly", {
  # Make poisson prior
  prior <- prior_poisson(lambda = 0.2)

  # Expect correct class
  expect_class(prior, "PriorPoisson")
  expect_equal(prior@lambda, 0.2)

  # Expect N inputs correct
  expect_equal(NROW(slotNames(prior)) - 3, prior@n_param)

  # Errors
  expect_error(prior_poisson(-1),
    regexp = "invalid class .PriorPoisson. object: lambda must be >0"
  )
})

test_that("show works for PriorPoisson", {
  expect_snapshot_output(show(prior_poisson(3)))
})

test_that("plot works for PriorPoisson", {
  vdiffr::expect_doppelganger(
    "prior_poisson_plot",
    plot(prior_poisson(3))
  )
})

test_that("constraints work for PriorPoisson", {
  expect_equal(eval_constraints(prior_poisson(3)), "<lower=0>")
})

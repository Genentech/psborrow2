test_that("add_covariates is working correctly", {

  # Make covariate objects
  covs1 <- add_covariates(
    covariates = c("a", "b", "c"),
    priors = list(
      a = normal_prior(0, 1000),
      b = normal_prior(-5, 20),
      c = normal_prior(-30, 1)
    )
  )

  covs2 <- add_covariates(
    covariates = c("a", "b", "c"),
    priors = normal_prior(0, 1000)
  )

  # Expect correct class
  expect_class(covs1, "Covariates")
  expect_class(covs2, "Covariates")

  expect_true(all(sapply(covs1@priors, inherits, "Prior")))
  expect_true(is(covs2@priors, "Prior"))

  # Errors
  expect_error(
    add_covariates(
      covariates = c("a", "b"),
      priors = list("a" = normal_prior(0, 20))
    ),
    "Either specify 1 prior distribution for all covariates, or specify a named list with 1 prior per covariate"
  )

  expect_error(
    add_covariates(
      covariates = c("a", "c"),
      priors = list(
        "a" = normal_prior(0, 20),
        "b" = normal_prior(0, 10)
      )
    ),
    "If a list is provided to specify priors, one prior per covariate must be provided."
  )

  expect_error(
    add_covariates(
      covariates = c("a", "c"),
      priors = list(
        "a" = 2,
        "c" = 3
      )
    ),
    "If a list is provided to specify priors, all priors must be of class `Prior`"
  )

  expect_error(
    add_covariates(covariates = c("a", "b"), 20),
    "priors argument must be a single object of class `Prior` or a named list of objects of class `Prior`"
  )

  expect_error(
    add_covariates(covariates = c(1, 2, 3), normal_prior(0, 1000)),
    "Must be of type 'character', not 'double'"
  )
})

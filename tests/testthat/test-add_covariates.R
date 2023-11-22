test_that("add_covariates works for list of priors", {
  # Make covariate objects
  covs1 <- add_covariates(
    covariates = c("a", "b", "c"),
    priors = list(
      a = prior_normal(0, 1000),
      b = prior_normal(-5, 20),
      c = prior_normal(-30, 1)
    )
  )

  expect_class(covs1, "Covariates")
  expect_list(covs1@priors, types = "Prior", len = 3)
})

test_that("add_covariates works for a single prior", {
  covs2 <- add_covariates(
    covariates = c("a", "b", "c"),
    priors = prior_normal(0, 1000)
  )

  expect_class(covs2, "Covariates")
  expect_class(covs2@priors, "Prior")
})

test_that("add_covariates fails for invalid prior specification", {
  expect_error(
    add_covariates(
      covariates = c("a", "b"),
      priors = list("a" = prior_normal(0, 20))
    ),
    "Either specify 1 prior distribution for all covariates, or specify a named list with 1 prior per covariate"
  )

  expect_error(
    add_covariates(
      covariates = c("a", "c"),
      priors = list(
        "a" = prior_normal(0, 20),
        "b" = prior_normal(0, 10)
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
    add_covariates(covariates = c(1, 2, 3), prior_normal(0, 1000)),
    "Must be of type 'character', not 'double'"
  )
})

test_that("get_vars works for Covariates", {
  expect_identical(
    get_vars(add_covariates(
      covariates = c("a", "b", "c"),
      priors = prior_normal(0, 1000)
    )),
    c("a", "b", "c")
  )
})

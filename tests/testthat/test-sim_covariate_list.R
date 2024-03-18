test_that("Incorrect inputs lead to errors", {
  # At the bottom is a list of covariate objects
  expect_error(
    sim_covariate_list(
      add_covariates(
        c("cov1", "cov2"),
        prior_normal(0, 1000)
      )
    ),
    'should be or extend class "list"'
  )

  expect_error(
    sim_covariate_list(
      list(
        scenario_one = c("cov1", "cov2")
      )
    ),
    "must be a list of `Covariate` objects \\(or `NULL`\\)"
  )

  # Borrowing list must be named
  expect_error(
    sim_covariate_list(
      list(
        add_covariates(
          c("cov1", "cov2"),
          prior_normal(0, 1000)
        )
      )
    ),
    "`covariate_list` must be named"
  )

  # All items must be named
  expect_error(
    sim_covariate_list(
      list(
        no_adjustment = NULL,
        add_covariates(
          c("cov1", "cov2"),
          prior_normal(0, 1000)
        )
      )
    ),
    "All items in `covariate_list` must be named"
  )

  # Names must be unique
  expect_error(
    sim_covariate_list(
      list(
        scenario_1 = NULL,
        scenario_1 = add_covariates(
          c("cov1", "cov2"),
          prior_normal(0, 1000)
        )
      )
    ),
    "All names supplied to `covariate_list` must be unique"
  )
})

test_that("Correct inputs successfully produce `SimCovariateList`", {
  expect_class(
    sim_covariate_list(
      list(
        "No adjustment" = NULL,
        "Full adjustment" = add_covariates(
          c("cov1", "cov2"),
          prior_normal(0, 1000)
        )
      )
    ),
    "SimCovariateList"
  )
})

test_that("Covariate `guide` is produced correctly", {
  covariate_obj1 <- sim_covariate_list(
    list(
      "No adjustment" = NULL,
      "Full adjustment" = add_covariates(
        c("cov1", "cov2"),
        prior_normal(0, 1000)
      )
    )
  )

  expect_equal(
    covariate_obj1@guide$covariate_scenario,
    c("No adjustment", "Full adjustment")
  )
  expect_class(covariate_obj1@guide, "data.frame")
  expect_equal(colnames(covariate_obj1@guide), "covariate_scenario")

  covariate_obj2 <- sim_covariate_list(
    list(
      "No adjustment" = NULL,
      "Cov1 and cov2" = add_covariates(
        c("cov1", "cov2"),
        prior_normal(0, 1000)
      ),
      "Cov1 only" = add_covariates(
        c("cov1"),
        prior_normal(0, 1000)
      )
    )
  )

  expect_equal(
    covariate_obj2@guide$covariate_scenario,
    c("No adjustment", "Cov1 and cov2", "Cov1 only")
  )
  expect_class(covariate_obj2@guide, "data.frame")
  expect_equal(colnames(covariate_obj2@guide), "covariate_scenario")
})



test_that("get_vars for `sim_covariate_list` works", {
  covariate_obj <- sim_covariate_list(
    list(
      "No adjustment" = NULL,
      "Full adjustment" = add_covariates(
        c("cov1", "cov2"),
        prior_normal(0, 1000)
      )
    )
  )
  expect_equal(c("cov1", "cov2"), get_vars(covariate_obj))
})

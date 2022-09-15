
test_that("Incorrect inputs lead to errors", {
  # At the bottom is a list of outcome objects
  expect_error(
    sim_outcome_list(
      exp_surv_dist(
        "time",
        "cnsr",
        normal_prior(0, 1000)
      )
    ),
    'should be or extend class "list"'
  )

  expect_error(
    sim_outcome_list(
      list(
        scenario_one = "Exponential"
      )
    ),
    "must be a list of `Outcome` objects"
  )

  # Outcome list must be named
  expect_error(
    sim_outcome_list(
      list(
        exp_surv_dist(
          "time",
          "cnsr",
          normal_prior(0, 1000)
        )
      )
    ),
    "`outcome_list` must be named"
  )

  # All items must be named
  expect_error(
    sim_outcome_list(
      list(
        exp = exp_surv_dist(
          "time",
          "cnsr",
          normal_prior(0, 1000)
        ),
        weib_ph_surv_dist(
          "time",
          "cnsr",
          normal_prior(0, 1000),
          normal_prior(0, 1000)
        )
      )
    ),
    "All items in `outcome_list` must be named"
  )

  # Names must be unique
  expect_error(
    sim_outcome_list(
      list(
        scenario_1 = exp_surv_dist(
          "time",
          "cnsr",
          normal_prior(0, 1000)
        ),
        scenario_1 = weib_ph_surv_dist(
          "time",
          "cnsr",
          normal_prior(0, 1000),
          normal_prior(0, 1000)
        )
      )
    ),
    "All names supplied to `outcome_list` must be unique"
  )
})

test_that("Correct inputs successfully produce `SimOutcomeList`", {
  expect_class(
    sim_outcome_list(
      list(
        "Weibull uninformative" = weib_ph_surv_dist(
          "time",
          "cnsr",
          normal_prior(0, 1000),
          normal_prior(0, 1000)
        ),
        "Weibull increasing hazard" = weib_ph_surv_dist(
          "time",
          "cnsr",
          normal_prior(5, 2),
          normal_prior(0, 1000)
        )
      )
    ),
    "SimOutcomeList"
  )
})


test_that("Outcome `guide` is produced correctly", {
  outcome_obj1 <- sim_outcome_list(
    list(
      "Weibull uninformative" = weib_ph_surv_dist(
        "time",
        "cnsr",
        normal_prior(0, 1000),
        normal_prior(0, 1000)
      ),
      "Weibull increasing hazard" = weib_ph_surv_dist(
        "time",
        "cnsr",
        normal_prior(5, 2),
        normal_prior(0, 1000)
      )
    )
  )

  expect_equal(
    outcome_obj1@guide$outcome_scenario,
    c("Weibull uninformative", "Weibull increasing hazard")
  )
  expect_class(outcome_obj1@guide, "data.frame")
  expect_equal(colnames(outcome_obj1@guide), "outcome_scenario")

  outcome_obj2 <- sim_outcome_list(
    list(
      "Weibull uninformative" = weib_ph_surv_dist(
        "time",
        "cnsr",
        normal_prior(0, 1000),
        normal_prior(0, 1000)
      ),
      "Weibull increasing hazard" = weib_ph_surv_dist(
        "time",
        "cnsr",
        normal_prior(5, 2),
        normal_prior(0, 1000)
      ),
      "Exponential" = exp_surv_dist(
        "time",
        "cnsr",
        normal_prior(0, 1000)
      )
    )
  )

  expect_equal(
    outcome_obj2@guide$outcome_scenario,
    c("Weibull uninformative", "Weibull increasing hazard", "Exponential")
  )
  expect_class(outcome_obj2@guide, "data.frame")
  expect_equal(colnames(outcome_obj2@guide), "outcome_scenario")
})
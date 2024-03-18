test_that("Incorrect inputs lead to errors", {
  # At the bottom is a list of Treatment objects
  expect_error(
    sim_treatment_list(
      treatment_details("trt", prior_normal(0, 100))
    ),
    'should be or extend class "list"'
  )

  expect_error(
    sim_treatment_list(
      list(
        scenario_one = "Treatment one"
      )
    ),
    "must be a list of `Treatment` objects"
  )

  # Treatment list must be named
  expect_error(
    sim_treatment_list(
      list(
        treatment_details("trt", prior_normal(0, 1000))
      )
    ),
    "`treatment_list` must be named"
  )

  # All items must be named
  expect_error(
    sim_treatment_list(
      list(
        uninformative = treatment_details("trt", prior_normal(0, 1000)),
        treatment_details("trt", prior_normal(-50, 20))
      )
    ),
    "All items in `treatment_list` must be named"
  )

  # Names must be unique
  expect_error(
    sim_treatment_list(
      list(
        scenario_1 = treatment_details("trt", prior_normal(0, 1000)),
        scenario_1 = treatment_details("trt", prior_normal(-50, 20))
      )
    ),
    "All names supplied to `treatment_list` must be unique"
  )
})

test_that("Correct inputs successfully produce `SimTreatmentList`", {
  expect_class(
    sim_treatment_list(
      list(
        uninformative = treatment_details("trt", prior_normal(0, 1000)),
        informative = treatment_details("trt", prior_normal(-50, 20))
      )
    ),
    "SimTreatmentList"
  )
})

test_that("Treatment `guide` is produced correctly", {
  treatment_obj1 <- sim_treatment_list(
    list(
      "Uninformative" = treatment_details("trt", prior_normal(0, 1000)),
      "Informative" = treatment_details("trt", prior_normal(-50, 20))
    )
  )

  expect_equal(
    treatment_obj1@guide$treatment_scenario,
    c("Uninformative", "Informative")
  )
  expect_class(treatment_obj1@guide, "data.frame")
  expect_equal(colnames(treatment_obj1@guide), "treatment_scenario")

  treatment_obj2 <- sim_treatment_list(
    list(
      "Uninformative" = treatment_details("trt", prior_normal(0, 1000)),
      "Informative - protective" = treatment_details("trt", prior_normal(-50, 20)),
      "Informative - adverse" = treatment_details("trt", prior_normal(50, 20))
    )
  )

  expect_equal(
    treatment_obj2@guide$treatment_scenario,
    c("Uninformative", "Informative - protective", "Informative - adverse")
  )
  expect_class(treatment_obj2@guide, "data.frame")
  expect_equal(colnames(treatment_obj2@guide), "treatment_scenario")
})

test_that("get_vars for `sim_treatment_list` works", {
  treatment_obj <- sim_treatment_list(
    list(
      "Uninformative" = treatment_details("trt", prior_normal(0, 1000)),
      "Informative" = treatment_details("trt", prior_normal(-50, 20))
    )
  )

  expect_equal("trt", get_vars(treatment_obj))
})

test_that("treatment_details works when inputs are correct", {
  # Make treatment arm objects
  sta <- treatment_details(
    trt_flag_col = "trt",
    trt_prior = prior_normal(0, 1000)
  )

  expect_class(sta, "Treatment")
  expect_class(sta@trt_prior, "Prior")
})

test_that("treatment_details fails fails for invalid prior and flag specification", {
  expect_error(
    treatment_details(
      trt_flag_col = "trt",
      trt_prior = "normal(0, 1000)"
    ),
    "Must inherit from class 'Prior', but has class 'character'."
  )

  expect_error(
    treatment_details(
      trt_flag_col = 2,
      trt_prior = prior_normal(0, 1000)
    ),
    "Must be of type 'string', not 'double'"
  )
})

test_that("get_vars works for treatment_details", {
  expect_identical(
    get_vars(treatment_details(
      trt_flag_col = "treat_fl",
      trt_prior = prior_normal(0, 1000)
    )),
    c(trt_flag_col = "treat_fl")
  )
})

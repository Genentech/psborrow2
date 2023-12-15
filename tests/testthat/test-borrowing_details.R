test_that("get_vars works for borrowing_details", {
  expect_identical(
    get_vars(borrowing_details(
      "BDB",
      "ext_fl",
      prior_gamma(.1, .1)
    )),
    c(ext_flag_col = "ext_fl")
  )
})

test_that("borrowing_details checks tau prior limits", {
  expect_error(
    borrowing_details(
      "BDB",
      "ext_fl",
      uniform_prior(-10, 10)
    ),
    "tau distribution must be bounded >=0"
  )
  expect_error(
    borrowing_details(
      "BDB",
      "ext_fl",
      prior_normal(0, 10000)
    ),
    "tau distribution must be bounded >=0"
  )
})

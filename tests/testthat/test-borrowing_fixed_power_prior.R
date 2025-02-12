test_that("borrowing_fixed_power_prior works as expected", {
  fpp <- borrowing_fixed_power_prior("ext", "pp")
  expect_class(fpp, "Borrowing")
  expect_class(fpp, "BorrowingFixedPowerPrior")
  expect_equal(fpp@ext_flag_col, "ext")
  expect_equal(fpp@power_col, "pp")
})

test_that("get_vars works for BorrowingFixedPowerPrior", {
  expect_identical(
    get_vars(borrowing_fixed_power_prior("ext_fl", "power")),
    c(ext_flag_col = "ext_fl", power_col = "power")
  )
})

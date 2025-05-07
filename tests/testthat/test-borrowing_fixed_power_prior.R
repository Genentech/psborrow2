test_that("borrowing_fixed_power_prior works as expected", {
  fpp <- borrowing_fixed_power_prior("ext", 0.33)
  expect_class(fpp, "Borrowing")
  expect_class(fpp, "BorrowingFixedPowerPrior")
  expect_equal(fpp@ext_flag_col, "ext")
  expect_equal(fpp@power_par, 0.33)
})

test_that("get_vars works for BorrowingFixedPowerPrior", {
  expect_identical(
    get_vars(borrowing_fixed_power_prior("ext_fl", 0.2)),
    c(ext_flag_col = "ext_fl")
  )
})

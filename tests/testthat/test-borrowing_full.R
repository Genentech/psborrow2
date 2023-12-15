test_that("borrowing_full works as expected for no borrowing", {
  nb <- borrowing_full("ext")
  expect_class(nb, "Borrowing")
  expect_class(nb, "BorrowingFull")
  expect_equal(nb@ext_flag_col, "ext")
})

test_that("borrowing_full rejects the tau prior", {
  expect_error(
    borrowing_full(ext_flag_col = "ext", prior_normal(0, 1)),
    "unused argument"
  )
})

test_that("get_vars works for borrowing_none", {
  expect_identical(
    get_vars(borrowing_full(
      "ext_fl"
    )),
    c(ext_flag_col = "ext_fl")
  )
})

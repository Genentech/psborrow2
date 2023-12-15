test_that("borrowing_full works as expected for no borrowing", {
  nb <- borrowing_full()
  expect_class(nb, "Borrowing")
  expect_class(nb, "BorrowingFull") 
  expect_error(nb@ext_flag_col, "no slot")
})

test_that("borrowing_full rejects all arguments", {
  expect_error(
    borrowing_full(ext_flag_col = "ext"),
    "unused argument"
  )
})

test_that("get_vars works for borrowing_full", {
  expect_identical(
    get_vars(borrowing_full()),
    c()
  )
})

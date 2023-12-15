test_that("borrowing_full works as expected for no borrowing", {
  nb <- borrowing_full()
  expect_class(nb, "Borrowing")
  expect_class(nb, "BorrowingFull") 
  expect_error(nb@ext_flag_col, "no slot")
})
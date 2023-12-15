test_that("borrowing_details is truly deprecated", {
  expect_error(
    borrowing_details(method = "BDB", ext_flag_col = "ext_fl", tau_prior = prior_gamma(.1, .1)),
    "deprecated"
  )
})

test_that("borrowing_none works as expected for no borrowing", {
  nb <- borrowing_none("ext")
  expect_class(nb, "Borrowing")
  expect_class(nb, "BorrowingNone") 
  expect_equal(nb@ext_flag_col, "ext")
})


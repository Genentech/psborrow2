test_that("borrowing_details is truly deprecated", {
  expect_error(
    borrowing_details(ext_flag_col = "ext_fl", tau_prior = prior_gamma(.1, .1)),
    "deprecated"
  )
})

test_that("borrowing_none works as expected for no borrowing", {
  nb <- borrowing_none("ext")
  expect_class(nb, "Borrowing")
  expect_class(nb, "BorrowingNone")
  expect_equal(nb@ext_flag_col, "ext")
})

test_that("borrowing_none rejects the tau prior", {
  expect_error(
    borrowing_none(ext_flag_col = "ext", prior_normal(0, 1)),
    "unused argument"
  )
})

test_that("borrowing_none requires ext_flag_col", {
  expect_error(
    borrowing_none(),
    "\"ext_flag_col\" is missing, with no default"
  )
})

test_that("get_vars works for borrowing_none", {
  expect_identical(
    get_vars(borrowing_none(
      "ext_fl"
    )),
    c(ext_flag_col = "ext_fl")
  )
})

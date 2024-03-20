test_that("borrowing_hierarchical_commensurate works as expected for no borrowing", {
  nb <- borrowing_hierarchical_commensurate(
    tau_prior = prior_gamma(alpha = .001, beta = .001),
    ext_flag_col = "ext"
  )
  expect_class(nb, "Borrowing")
  expect_class(nb, "BorrowingHierarchicalCommensurate")
  expect_equal(nb@ext_flag_col, "ext")
  expect_equal(nb@tau_prior, prior_gamma(alpha = .001, beta = .001))
})

test_that("borrowing_hierarchical_commensurate requires the tau prior", {
  expect_error(
    borrowing_hierarchical_commensurate(ext_flag_col = "ext"),
    "\"tau_prior\" is missing, with no default"
  )
})

test_that("borrowing_hierarchical_commensurate requires the ext flag col", {
  expect_error(
    borrowing_hierarchical_commensurate(tau_prior = prior_gamma(alpha = .001, beta = .001)),
    "\"ext_flag_col\" is missing, with no default"
  )
})

test_that("get_vars works for borrowing_hierarchical_commensurate", {
  expect_identical(
    get_vars(borrowing_hierarchical_commensurate(
      "ext_fl",
      prior_gamma(.1, .1)
    )),
    c(ext_flag_col = "ext_fl")
  )
})

test_that("borrowing_hierarchical_commensurate checks tau prior limits", {
  expect_error(
    borrowing_hierarchical_commensurate(
      "ext_fl",
      uniform_prior(-10, 10)
    ),
    "tau distribution must be bounded >=0"
  )
  expect_error(
    borrowing_hierarchical_commensurate(
      "ext_fl",
      prior_normal(0, 10000)
    ),
    "tau distribution must be bounded >=0"
  )
})

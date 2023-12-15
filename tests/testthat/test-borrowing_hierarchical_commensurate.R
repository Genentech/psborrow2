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
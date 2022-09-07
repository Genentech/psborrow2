test_that("cont_var functions correctly", {
  # Correct class
  cv <- cont_var(1, 2)
  expect_class(cv, "SimVarCont")

  # Errors on non-numeric input
  expect_error(
    cont_var("one", "two"),
    "Must be of type 'numeric', not 'character'"
  )

  # Errors on wrong number inputs
  expect_error(
    cont_var(1),
    "Argument 'mu_external' is missing"
  )
})

test_that("bin_var functions correctly", {
  # Correct class
  bv <- bin_var(0.5, 0.5)
  expect_class(bv, "SimVarBin")

  # Errors on non-numeric input
  expect_error(
    bin_var(0.9, 1.2),
    "`prob_external` must be \\[0, 1\\]"
  )

  # Errors on wrong number inputs
  expect_error(
    bin_var(0.99),
    "Argument 'prob_external' is missing"
  )
})

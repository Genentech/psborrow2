test_that("borrowing_details works as expected", {
  # Create objects
  fb <- borrowing_details("Full borrowing", normal_prior(0, 1000))
  bdb <- borrowing_details("BDB", normal_prior(0, 1000), "ext", gamma_prior(.1, .1))

  # Check classes
  expect_class(fb, "Borrowing")
  expect_class(bdb, "Borrowing")

  # See that the columns were added
  expect_equal(fb@method, "Full borrowing")
  expect_class(bdb@tau_prior, "GammaPrior")

  # Errors
  expect_error(
    borrowing_details("Dr Dre", normal_prior(0, 1)),
    "method must be within"
  )
  expect_error(
    borrowing_details("Full borrowing",
      normal_prior(0, 100),
      tau_prior = normal_prior(0, 1000)
    ),
    "no need to specify tau prior when method is not BDB"
  )
  expect_error(borrowing_details("BDB"), "must be specified")
})

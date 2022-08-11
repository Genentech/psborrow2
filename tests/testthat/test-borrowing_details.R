test_that("borrowing_details works as expected", {
  # Create objects
  fb <- borrowing_details(
    "Full borrowing",
    normal_prior(0, 1000),
    "ext"
  )

  bdb <- borrowing_details(
    "BDB",
    normal_prior(0, 1000),
    "ext",
    gamma_prior(.1, .1)
  )

  nb <- borrowing_details(
    "No borrowing",
    normal_prior(0, 1000),
    "ext"
  )

  # Check classes
  expect_class(fb, "Borrowing")
  expect_class(bdb, "Borrowing")
  expect_class(nb, "Borrowing")

  # See that the columns were added
  expect_equal(fb@method, "Full borrowing")
  expect_class(bdb@tau_prior, "GammaPrior")
  expect_equal(nb@ext_flag_col, "ext")

  # Errors
  expect_error(
    borrowing_details("no Borrowing", normal_prior(0, 1)),
    "Must be element of set"
  )

  expect_error(
    borrowing_details("Full borrowing",
      normal_prior(0, 100),
      tau_prior = normal_prior(0, 1000)
    ),
    "ext_flag_col must be specified"
  )

  expect_error(
    borrowing_details("Full borrowing",
      baseline_prior = "Normal",
      ext_flag_col = "IMbrave150"
    ),
    'should be or extend class "Prior"'
  )

  expect_error(borrowing_details("BDB"), "must be specified")

  # Message
  expect_message(
    borrowing_details(
      method = "No borrowing",
      baseline_prior = normal_prior(0, 1),
      "ext"
    ),
    "Filtering model matrix"
  )

  expect_message(
    borrowing_details(
      method = "Full borrowing",
      baseline_prior = normal_prior(0, 1),
      "ext"
    ),
    "Ignoring ext_flag_col for full borrowing"
  )
})

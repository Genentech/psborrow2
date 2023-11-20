test_that("borrowing_details works as expected for no borrowing", {
  nb <- borrowing_details(
    "No borrowing",
    "ext"
  )

  expect_class(nb, "Borrowing")
  expect_equal(nb@method, "No borrowing")
  expect_equal(nb@ext_flag_col, "ext")
})


test_that("borrowing_details works as expected for full borrowing", {
  fb <- borrowing_details(
    "Full borrowing",
    "ext"
  )

  expect_class(fb, "Borrowing")
  expect_equal(fb@method, "Full borrowing")
  expect_equal(fb@ext_flag_col, "ext")
})


test_that("borrowing_details works as expected for BDB", {
  bdb <- borrowing_details(
    "BDB",
    "ext",
    gamma_prior(.1, .1)
  )

  expect_class(bdb, "Borrowing")
  expect_equal(bdb@method, "BDB")
  expect_class(bdb@tau_prior, "GammaPrior")
  expect_equal(bdb@ext_flag_col, "ext")
})


test_that("borrowing_details gives expected errors", {
  expect_error(
    borrowing_details("no Borrowing", prior_normal(0, 1)),
    "Must be element of set"
  )

  expect_error(
    borrowing_details(
      "Full borrowing",
      tau_prior = prior_normal(0, 1000)
    ),
    "missing, with no default"
  )

  expect_error(borrowing_details("BDB"), "missing, with no default")
})

test_that("get_vars works for borrowing_details", {
  expect_identical(
    get_vars(borrowing_details(
      "BDB",
      "ext_fl",
      gamma_prior(.1, .1)
    )),
    c(ext_flag_col = "ext_fl")
  )
})

test_that("borrowing_details checks tau prior limits", {
  expect_error(
    borrowing_details(
      "BDB",
      "ext_fl",
      uniform_prior(-10, 10)
    ),
    "tau distribution must be bounded >=0"
  )
  expect_error(
    borrowing_details(
      "BDB",
      "ext_fl",
      prior_normal(0, 10000)
    ),
    "tau distribution must be bounded >=0"
  )
})

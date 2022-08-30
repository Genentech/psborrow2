test_that("borrowing_details works as expected for no borrowing", {
  nb <- borrowing_details(
    "No borrowing",
    normal_prior(0, 1000),
    "ext"
  )

  expect_class(nb, "Borrowing")
  expect_equal(nb@method, "No borrowing")
  expect_equal(nb@ext_flag_col, "ext")
})


test_that("borrowing_details works as expected for full borrowing", {
  fb <- borrowing_details(
    "Full borrowing",
    normal_prior(0, 1000),
    "ext"
  )

  expect_class(fb, "Borrowing")
  expect_equal(fb@method, "Full borrowing")
  expect_equal(fb@ext_flag_col, "ext")
  expect_equal(fb@baseline_prior, normal_prior(0, 1000))
})


test_that("borrowing_details works as expected for BDB", {
  bdb <- borrowing_details(
    "BDB",
    normal_prior(0, 1000),
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
    borrowing_details("no Borrowing", normal_prior(0, 1)),
    "Must be element of set"
  )

  expect_error(
    borrowing_details(
      "Full borrowing",
      normal_prior(0, 100),
      tau_prior = normal_prior(0, 1000)
    ),
    "missing, with no default"
  )

  expect_error(
    borrowing_details(
      "Full borrowing",
      baseline_prior = "Normal",
      ext_flag_col = "IMbrave150"
    ),
    "Must inherit from class 'Prior'"
  )

  expect_error(borrowing_details("BDB"), "missing, with no default")
})

test_that("get_vars works for borrowing_details", {
  expect_identical(
    get_vars(borrowing_details(
      "BDB",
      normal_prior(0, 1000),
      "ext_fl",
      gamma_prior(.1, .1)
    )),
    c(ext_flag_col = "ext_fl")
  )
})

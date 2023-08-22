test_that("Incorrect inputs lead to errors", {
  # At the bottom is a list of borrowing objects
  expect_error(
    sim_borrowing_list(
      borrowing_details(
        method = "BDB_HCP",
        ext_flag_col = "ext",
        tau_prior = exponential_prior(0.001)
      )
    ),
    'should be or extend class "list"'
  )

  expect_error(
    sim_borrowing_list(
      list(
        scenario_one = "Full borrowing"
      )
    ),
    "must be a list of `Borrowing` objects"
  )

  # Borrowing list must be named
  expect_error(
    sim_borrowing_list(
      list(
        borrowing_details(
          method = "BDB_HCP",
          ext_flag_col = "ext",
          tau_prior = exponential_prior(0.001)
        )
      )
    ),
    "`borrowing_list` must be named"
  )

  # All items must be named
  expect_error(
    sim_borrowing_list(
      list(
        bdb = borrowing_details(
          method = "BDB_HCP",
          ext_flag_col = "ext",
          tau_prior = exponential_prior(0.001)
        ),
        borrowing_details(
          method = "Full borrowing",
          "ext"
        )
      )
    ),
    "All items in `borrowing_list` must be named"
  )

  # Names must be unique
  expect_error(
    sim_borrowing_list(
      list(
        scenario_1 = borrowing_details(
          method = "BDB_HCP",
          ext_flag_col = "ext",
          tau_prior = exponential_prior(0.001)
        ),
        scenario_1 = borrowing_details(
          method = "Full borrowing",
          "ext"
        )
      )
    ),
    "All names supplied to `borrowing_list` must be unique"
  )
})

test_that("Correct inputs successfully produce `SimBorrowingList`", {
  expect_class(
    sim_borrowing_list(
      list(
        "BDB_HCP" = borrowing_details(
          method = "BDB_HCP",
          ext_flag_col = "ext",
          tau_prior = exponential_prior(0.001)
        ),
        "Full borrowing" = borrowing_details(
          method = "Full borrowing",
          "ext"
        )
      )
    ),
    "SimBorrowingList"
  )
})


test_that("Borrowing `guide` is produced correctly", {
  borrowing_obj1 <- sim_borrowing_list(
    list(
      "BDB_HCP" = borrowing_details(
        method = "BDB_HCP",
        ext_flag_col = "ext",
        tau_prior = exponential_prior(0.001)
      ),
      "Full borrowing" = borrowing_details(
        method = "Full borrowing",
        "ext"
      )
    )
  )

  expect_equal(
    borrowing_obj1@guide$borrowing_scenario,
    c("BDB_HCP", "Full borrowing")
  )
  expect_class(borrowing_obj1@guide, "data.frame")
  expect_equal(colnames(borrowing_obj1@guide), "borrowing_scenario")

  borrowing_obj2 <- sim_borrowing_list(
    list(
      "BDB_HCP" = borrowing_details(
        method = "BDB_HCP",
        ext_flag_col = "ext",
        tau_prior = exponential_prior(0.001)
      ),
      "Full borrowing" = borrowing_details(
        method = "Full borrowing",
        "ext"
      ),
      "No borrowing" = borrowing_details(method = "No borrowing", "ext")
    )
  )

  expect_equal(
    borrowing_obj2@guide$borrowing_scenario,
    c("BDB_HCP", "Full borrowing", "No borrowing")
  )
  expect_class(borrowing_obj2@guide, "data.frame")
  expect_equal(colnames(borrowing_obj2@guide), "borrowing_scenario")
})


test_that("get_vars for `sim_borrowing_list` works", {
  borrowing_obj <- sim_borrowing_list(
    list(
      "BDB_HCP" = borrowing_details(
        method = "BDB_HCP",
        ext_flag_col = "ext",
        tau_prior = exponential_prior(0.001)
      ),
      "Full borrowing" = borrowing_details(
        method = "Full borrowing",
        "ext"
      )
    )
  )

  expect_equal("ext", get_vars(borrowing_obj))
})

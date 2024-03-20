test_that("Incorrect inputs lead to errors", {
  # At the bottom is a list of borrowing objects
  expect_error(
    sim_borrowing_list(
      borrowing_hierarchical_commensurate(
        ext_flag_col = "ext",
        tau_prior = prior_exponential(0.001)
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
        borrowing_hierarchical_commensurate(
          ext_flag_col = "ext",
          tau_prior = prior_exponential(0.001)
        )
      )
    ),
    "`borrowing_list` must be named"
  )

  # All items must be named
  expect_error(
    sim_borrowing_list(
      list(
        bdb = borrowing_hierarchical_commensurate(
          ext_flag_col = "ext",
          tau_prior = prior_exponential(0.001)
        ),
        borrowing_full("ext")
      )
    ),
    "All items in `borrowing_list` must be named"
  )

  # Names must be unique
  expect_error(
    sim_borrowing_list(
      list(
        scenario_1 = borrowing_hierarchical_commensurate(
          ext_flag_col = "ext",
          tau_prior = prior_exponential(0.001)
        ),
        scenario_1 = borrowing_full("ext")
      )
    ),
    "All names supplied to `borrowing_list` must be unique"
  )
})

test_that("Correct inputs successfully produce `SimBorrowingList`", {
  expect_class(
    sim_borrowing_list(
      list(
        "BDB" = borrowing_hierarchical_commensurate(
          ext_flag_col = "ext",
          tau_prior = prior_exponential(0.001)
        ),
        "Full borrowing" = borrowing_full("ext")
      )
    ),
    "SimBorrowingList"
  )
})


test_that("Borrowing `guide` is produced correctly", {
  borrowing_obj1 <- sim_borrowing_list(
    list(
      "BDB" = borrowing_hierarchical_commensurate(
        ext_flag_col = "ext",
        tau_prior = prior_exponential(0.001)
      ),
      "Full borrowing" = borrowing_full("ext")
    )
  )

  expect_equal(
    borrowing_obj1@guide$borrowing_scenario,
    c("BDB", "Full borrowing")
  )
  expect_class(borrowing_obj1@guide, "data.frame")
  expect_equal(colnames(borrowing_obj1@guide), "borrowing_scenario")

  borrowing_obj2 <- sim_borrowing_list(
    list(
      "BDB" = borrowing_hierarchical_commensurate(
        ext_flag_col = "ext",
        tau_prior = prior_exponential(0.001)
      ),
      "Full borrowing" = borrowing_full("ext"),
      "No borrowing" = borrowing_none("ext")
    )
  )

  expect_equal(
    borrowing_obj2@guide$borrowing_scenario,
    c("BDB", "Full borrowing", "No borrowing")
  )
  expect_class(borrowing_obj2@guide, "data.frame")
  expect_equal(colnames(borrowing_obj2@guide), "borrowing_scenario")
})


test_that("get_vars for `sim_borrowing_list` works", {
  borrowing_obj <- sim_borrowing_list(
    list(
      "BDB" = borrowing_hierarchical_commensurate(
        ext_flag_col = "ext",
        tau_prior = prior_exponential(0.001)
      ),
      "Full borrowing" = borrowing_full("ext")
    )
  )

  expect_equal("ext", get_vars(borrowing_obj))
})

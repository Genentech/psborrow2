base_mat <- matrix(
  c(
    rep(0, 200), rep(0, 200), rep(1, 200),
    rep(1, 200), rep(0, 200), rep(0, 200),
    rep(0, 600)
  ),
  ncol = 3,
  dimnames = list(NULL, c("ext", "trt", "driftOR"))
)

add_binary_endpoint <- function(odds_ratio,
                                base_matrix = base_mat) {
  linear_predictor <- base_matrix[, "trt"] * log(odds_ratio)
  prob <- 1 / (1 + exp(-linear_predictor))

  bin_endpoint <- rbinom(
    NROW(base_matrix),
    1,
    prob
  )

  cbind(base_matrix, matrix(bin_endpoint, ncol = 1, dimnames = list(NULL, "ep")))
}

data_list <- list(
  list(add_binary_endpoint(1.5), add_binary_endpoint(1.5)),
  list(add_binary_endpoint(2.5), add_binary_endpoint(2.5))
)

test_that("Incorrect inputs lead to errors", {
  # Format is list of lists
  short_list <- list(add_binary_endpoint(1.5), add_binary_endpoint(2.5))
  expect_error(
    sim_data_list(
      short_list,
      data.frame(
        trueOR = c(1.5, 2.5),
        driftOR = c(1.0, 1.0)
      ),
      "trueOR",
      "driftOR"
    ),
    "must be a list of lists"
  )

  # At lowest level there are matrices
  data_list_df <- list(
    list(as.data.frame(add_binary_endpoint(1.5)), as.data.frame(add_binary_endpoint(1.5)))
  )
  expect_error(
    sim_data_list(
      data_list_df,
      data.frame(
        trueOR = c(1.5),
        driftOR = c(1.0)
      ),
      "trueOR",
      "driftOR"
    ),
    "must be matrices"
  )

  # Guide and data list are same length
  expect_error(
    sim_data_list(
      data_list,
      data.frame(
        trueOR = c(1.5),
        driftOR = c(1.0)
      ),
      "trueOR",
      "driftOR"
    ),
    "must be same length"
  )

  # Effect and drift are columns in guide
  expect_error(
    sim_data_list(
      data_list,
      data.frame(
        trueOR = c(1.5, 2.5),
        driftOR = c(1.0, 1.0)
      ),
      "OR",
      "driftOR"
    ),
    "must be a column"
  )

  expect_error(
    sim_data_list(
      data_list,
      data.frame(
        trueOR = c(1.5, 2.5),
        driftOR = c(1.0, 1.0)
      ),
      "trueOR",
      "drift"
    ),
    "must be a column"
  )

  expect_error(
    sim_data_list(
      data_list,
      data.frame(
        trueOR = c(1.5, 2.5),
        driftOR = c(1.0, 1.0),
        n_datasets_per_param = c(2L, 2L)
      ),
      "trueOR",
      "driftOR"
    ),
    "'n_datasets_per_param' is a protected column name in `guide`"
  )
})


test_that("Correct inputs successfully produce `SimDataList`", {
  expect_class(
    sim_data_list(
      data_list,
      data.frame(
        trueOR = c(1.5, 2.5),
        driftOR = c(1.0, 1.0)
      ),
      "trueOR",
      "driftOR"
    ),
    "SimDataList"
  )
})

test_that("N datasets per param is correct", {
  expect_equal(
    sim_data_list(
      data_list,
      data.frame(
        trueOR = c(1.5, 2.5),
        driftOR = c(1.0, 1.0)
      ),
      "trueOR",
      "driftOR"
    )@guide$n_datasets_per_param,
    c(2, 2)
  )
})

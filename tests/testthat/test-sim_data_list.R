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
        driftOR = c(1.0, 1.0),
        index = 1L:2L
      ),
      "trueOR",
      "driftOR",
      "index"
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
        driftOR = c(1.0),
        index = 1L:2L
      ),
      "trueOR",
      "driftOR",
      "index"
    ),
    "must be matrices"
  )

  # Guide and data list are same length
  expect_error(
    sim_data_list(
      data_list,
      data.frame(
        trueOR = c(1.5),
        driftOR = c(1.0),
        index = 1L
      ),
      "trueOR",
      "driftOR",
      "index"
    ),
    "must be same length"
  )

  # Effect and drift are columns in guide
  expect_error(
    sim_data_list(
      data_list,
      data.frame(
        trueOR = c(1.5, 2.5),
        driftOR = c(1.0, 1.0),
        index = as.integer(1:2)
      ),
      "OR",
      "driftOR",
      "index"
    ),
    "must be a column"
  )

  expect_error(
    sim_data_list(
      data_list,
      data.frame(
        trueOR = c(1.5, 2.5),
        driftOR = c(1.0, 1.0),
        index = as.integer(1:2)
      ),
      "trueOR",
      "drift",
      "index"
    ),
    "must be a column"
  )

  expect_error(
    sim_data_list(
      data_list,
      data.frame(
        trueOR = c(1.5, 2.5),
        driftOR = c(1.0, 1.0),
        n_datasets_per_param = c(2L, 2L),
        index = 1L:2L
      ),
      "trueOR",
      "driftOR",
      "index"
    ),
    "'n_datasets_per_param' is a protected column name in `guide`"
  )

  expect_error(
    sim_data_list(
      data_list,
      data.frame(
        trueOR = c(1.5, 2.5),
        driftOR = c(1.0, 1.0),
        index = 1L:2L
      ),
      "trueOR",
      "driftOR",
      "windex"
    ),
    "`index` must be a column in `guide`"
  )

  expect_error(
    sim_data_list(
      data_list,
      data.frame(
        trueOR = c(1.5, 2.5),
        driftOR = c(1.0, 1.0),
        index = 1L:1L
      ),
      "trueOR",
      "driftOR",
      "index"
    ),
    "must be unique"
  )

  expect_error(
    sim_data_list(
      data_list,
      data.frame(
        trueOR = c(1.5, 2.5),
        driftOR = c(1.0, 1.0),
        index = c(1.000, 2.000001)
      ),
      "trueOR",
      "driftOR",
      "index"
    ),
    "must of type integer"
  )
})

test_that("Correct inputs successfully produce `SimDataList`", {
  expect_class(
    sim_data_list(
      data_list,
      data.frame(
        trueOR = c(1.5, 2.5),
        driftOR = c(1.0, 1.0),
        index = 1L:2L
      ),
      "trueOR",
      "driftOR",
      "index"
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
        driftOR = c(1.0, 1.0),
        index = as.integer(1:2)
      ),
      "trueOR",
      "driftOR",
      "index"
    )@guide$n_datasets_per_param,
    c(2, 2)
  )
})


test_that("Combining SimDataLists works correctly with good arguments", {
  list_1 <- sim_data_list(
    data_list,
    data.frame(
      trueOR = c(1.5, 2.5),
      driftOR = c(1.0, 1.0),
      index = 1L:2L
    ),
    "trueOR",
    "driftOR",
    "index"
  )

  data_list2 <- lapply(c(1, 1, 2), function(i) data_list[[i]][c(1, 1, 2)])
  list_2 <- sim_data_list(
    data_list2,
    data.frame(
      trueOR = c(3.5, 4.5, 5.5),
      driftOR = c(1.1, 1.1, 1.1),
      index = 1L:3L
    ),
    "trueOR",
    "driftOR",
    "index"
  )

  result <- c(list_1, list_2)
  expect_class(result, "SimDataList")
  expect_list(result@data_list, len = 5)
  expect_equal(result@guide$trueOR, c(1.5, 2.5, 3.5, 4.5, 5.5))
  expect_equal(result@guide$driftOR, c(1.0, 1.0, 1.1, 1.1, 1.1))
  expect_equal(result@guide$index, 1:5)
  expect_equal(result@guide$n_datasets_per_param, c(2, 2, 3, 3, 3))
})


test_that("Combining SimDataLists gives error with other classes", {
  object <- sim_data_list(
    data_list,
    data.frame(
      trueOR = c(1.5, 2.5),
      driftOR = c(1.0, 1.0),
      index = 1L:2L
    ),
    "trueOR",
    "driftOR",
    "index"
  )
  expect_error(
    c(object, list(data.frame(id = 1, drift = 2))),
    "May only contain the following types: .SimDataList., but element 1 has type .list."
  )
})

test_that("Combining SimDataLists gives error if slots don't match", {
  list_1 <- sim_data_list(
    data_list,
    data.frame(
      trueOR = c(1.5, 2.5),
      driftOR = c(1.0, 1.0),
      index = 1L:2L
    ),
    "trueOR",
    "driftOR",
    "index"
  )

  list_2 <- sim_data_list(
    data_list,
    data.frame(
      true_OR = c(1.5, 2.5),
      drift_OR = c(1.0, 1.0),
      index = 1L:2L
    ),
    "true_OR",
    "drift_OR",
    "index"
  )

  expect_error(c(list_1, list_2), "Colnames", fixed = TRUE)
  expect_error(c(list_1, list_2), "@drift", fixed = TRUE)
  expect_error(c(list_1, list_2), "@effect", fixed = TRUE)
})

test_that("Combining SimDataLists gives error if guide columns don't match", {
  list_1 <- sim_data_list(
    data_list,
    data.frame(
      trueOR = c(1.5, 2.5),
      driftOR = c(1.0, 1.0),
      index = 1L:2L
    ),
    "trueOR",
    "driftOR",
    "index"
  )

  list_2 <- sim_data_list(
    data_list,
    data.frame(
      trueOR = c(1.5, 2.5),
      driftOR = c(1.0, 1.0),
      index = 1L:2L,
      label = c("setting 1", "setting 2")
    ),
    "trueOR",
    "driftOR",
    "index"
  )
  expect_error(c(list_1, list_2), "identical")
})

test_that("Combining SimDataLists gives error if identical scenarios", {
  list_1 <- sim_data_list(
    data_list,
    data.frame(
      trueOR = c(1.5, 2.5),
      driftOR = c(1.0, 1.0),
      index = 1L:2L
    ),
    "trueOR",
    "driftOR",
    "index"
  )

  list_2 <- sim_data_list(
    data_list,
    data.frame(
      trueOR = c(1.25, 2.25),
      driftOR = c(1.2, 1.2),
      index = 1L:2L
    ),
    "trueOR",
    "driftOR",
    "index"
  )

  list_3 <- sim_data_list(
    data_list,
    data.frame(
      trueOR = c(1.25, 2.5),
      driftOR = c(1.2, 1.0),
      index = 1L:2L
    ),
    "trueOR",
    "driftOR",
    "index"
  )

  expect_error(c(list_1, list_1), "Duplicate")
  expect_error(c(list_1, list_1, list_2), "Duplicate")
  expect_error(c(list_1, list_3), "Duplicate")
})

test_that("get_data works", {
  sim_data_list <- sim_data_list(
    data_list,
    data.frame(
      trueOR = c(1.5, 2.5),
      driftOR = c(1.0, 1.0),
      index = 1L:2L
    ),
    "trueOR",
    "driftOR",
    "index"
  )

  expect_equal(get_data(sim_data_list, 1), data_list[[1]])
  expect_equal(get_data(sim_data_list, 2), data_list[[2]])
  expect_equal(get_data(sim_data_list, 2, 1), data_list[[2]][[1]])
  expect_equal(get_data(sim_data_list), data_list)
})

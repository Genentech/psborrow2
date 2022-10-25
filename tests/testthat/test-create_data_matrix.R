test_that("create_data_matrix works", {
  dat <- survival::diabetic
  dat$ext <- dat$trt == 0 & dat$id > 1000
  result <- create_data_matrix(
    dat,
    outcome = c("time", "status"),
    trt_flag_col = "trt",
    ext_flag_col = "ext",
    covariates = ~ age + laser + risk
  )
  expect_matrix(
    result,
    nrows = 394,
    ncol = 7
  )
  expect_set_equal(
    colnames(result),
    c("time", "status", "trt", "extTRUE", "age", "laserargon", "risk")
  )

  result_char <- create_data_matrix(
    dat,
    outcome = c("time", "status"),
    trt_flag_col = "trt",
    ext_flag_col = "ext",
    covariates = c("age", "laser", "risk")
  )
  expect_identical(result, result_char)
})

test_that("create_data_matrix works with complex formulas", {
  dat <- survival::diabetic
  dat$ext <- dat$trt == 0 & dat$id > 1000
  result <- create_data_matrix(
    dat,
    outcome = c("time", "status"),
    trt_flag_col = "trt",
    ext_flag_col = "ext",
    covariates = ~ I(age^2) + log(risk) + laser * risk
  )
  expect_set_equal(
    colnames(result),
    c("time", "status", "trt", "extTRUE", "I(age^2)", "log(risk)", "laserargon", "risk", "laserargon:risk")
  )
})


test_that("create_data_matrix works with weights", {
  dat <- survival::diabetic
  dat$ext <- dat$trt == 0 & dat$id > 1000
  dat$weight <- runif(nrow(dat))
  result <- create_data_matrix(
    dat,
    outcome = c("time", "status"),
    trt_flag_col = "trt",
    ext_flag_col = "ext",
    covariates = ~laser,
    weight_var = "weight"
  )
  expect_set_equal(
    colnames(result),
    c("time", "status", "trt", "extTRUE", "laserargon", "weight")
  )
})

test_that("create_data_matrix works with one covariate", {
  dat <- survival::diabetic
  dat$ext <- dat$trt == 0 & dat$id > 1000
  result <- create_data_matrix(
    dat,
    outcome = c("time", "status"),
    trt_flag_col = "trt",
    ext_flag_col = "ext",
    covariates = ~laser
  )
  expect_set_equal(
    colnames(result),
    c("time", "status", "trt", "extTRUE", "laserargon")
  )
})


test_that("create_data_matrix works with no covariates", {
  dat <- survival::diabetic
  dat$ext <- dat$trt == 0 & dat$id > 1000
  result <- create_data_matrix(
    dat,
    outcome = c("time", "status"),
    trt_flag_col = "trt",
    ext_flag_col = "ext"
  )
  expect_matrix(result, nrow = 394)
  expect_set_equal(colnames(result), c("time", "status", "trt", "extTRUE"))
})


test_that("create_data_matrix gives error invalid outcome", {
  dat <- survival::diabetic
  dat$ext <- dat$trt == 0 & dat$id > 1000
  expect_error(
    create_data_matrix(
      dat,
      outcome = "OUTCOME",
      trt_flag_col = "trt",
      ext_flag_col = "ext",
      covariates = "age"
    ),
    "Must be a subset of"
  )
  expect_error(
    create_data_matrix(
      dat,
      outcome = NULL,
      trt_flag_col = "trt",
      ext_flag_col = "ext",
      covariates = "age"
    ),
    "Must be of type 'character'"
  )

  expect_error(
    create_data_matrix(
      dat,
      outcome = c("age", "laser", "risk"),
      trt_flag_col = "trt",
      ext_flag_col = "ext",
      covariates = "age"
    ),
    "Must have length <= 2"
  )
})

test_that("create_data_matrix gives error invalid trt_flag_col", {
  dat <- survival::diabetic
  dat$ext <- dat$trt == 0 & dat$id > 1000
  expect_error(
    create_data_matrix(
      dat,
      outcome = c("time", "status"),
      trt_flag_col = "TREATMENT",
      ext_flag_col = "ext",
      covariates = "age"
    ),
    "Must be a subset of"
  )
})

test_that("create_data_matrix gives error invalid ext_flag_col", {
  dat <- survival::diabetic
  dat$ext <- dat$trt == 0 & dat$id > 1000
  expect_error(
    create_data_matrix(
      dat,
      outcome = c("time", "status"),
      trt_flag_col = "trt",
      ext_flag_col = "EXTERNAL",
      covariates = "age"
    ),
    "Must be a subset of"
  )
})

test_that("create_data_matrix gives error invalid covariates", {
  dat <- survival::diabetic
  dat$ext <- dat$trt == 0 & dat$id > 1000
  expect_error(
    create_data_matrix(
      dat,
      outcome = c("time", "status"),
      trt_flag_col = "trt",
      ext_flag_col = "ext",
      covariates = "COVARIATE"
    ),
    "Must be a subset of"
  )

  expect_error(
    create_data_matrix(
      dat,
      outcome = c("time", "status"),
      trt_flag_col = "trt",
      ext_flag_col = "ext",
      covariates = ~COVARIATE
    ),
    "ust be a subset of"
  )

  expect_error(
    create_data_matrix(
      dat,
      outcome = c("time", "status"),
      trt_flag_col = "trt",
      ext_flag_col = "ext",
      covariates = "~age"
    ),
    "Must be a subset of"
  )

  expect_error(
    create_data_matrix(
      dat,
      outcome = c("time", "status"),
      trt_flag_col = "trt",
      ext_flag_col = "ext",
      covariates = 1:3
    ),
    "Must inherit from class 'character'/'formula'"
  )
})

test_that("create_data_matrix gives error for invalid data", {
  expect_error(
    create_data_matrix(
      letters,
      outcome = "OUTCOME",
      trt_flag_col = "trt",
      ext_flag_col = "ext",
      covariates = "age"
    ),
    "Must be of type 'data.frame'"
  )

  expect_error(
    create_data_matrix(
      diag(10),
      outcome = "OUTCOME",
      trt_flag_col = "trt",
      ext_flag_col = "ext",
      covariates = "age"
    ),
    "Must be of type 'data.frame'"
  )
})

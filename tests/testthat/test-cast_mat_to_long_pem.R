
anls <- psborrow2:::.analysis_obj(
     data_matrix = example_matrix,
     outcome = outcome_surv_pem(
       "time",
       "cnsr",
       baseline_prior = prior_normal(0, 1000),
       cut_points = c(1,2,3)
     ),
     borrowing = borrowing_hierarchical_commensurate(
       "ext",
       prior_exponential(.001)
     ),
     treatment = treatment_details(
       "trt",
       prior_normal(0, 1000)
     ),
     covariates = add_covariates(
       covariates = c("cov1", "cov2"),
       priors = prior_normal(0, 1000)
     )
   )

test_that("cast_mat_to_long_pem returns a matrix", {
  result <- cast_mat_to_long_pem(anls)
  expect_true(is.matrix(result))
})

test_that("cast_mat_to_long_pem handles cut points correctly", {
  result <- cast_mat_to_long_pem(anls)
  cut_points <- anls@outcome@cut_points
  max_fup <- max(anls@data_matrix[, anls@outcome@time_var])
  cut_points_keep <- cut_points[cut_points < max_fup]
  expect_warning(cast_mat_to_long_pem(anls), "Some cut points are greater than the maximum follow-up time")
  expect_equal(length(cut_points_keep), sum(cut_points < max_fup))
})

test_that("cast_mat_to_long_pem transforms data correctly", {
  result <- cast_mat_to_long_pem(anls)
  df <- as.data.frame(anls@data_matrix)
  expect_true(all(colnames(result) %in% c(colnames(df), "psb2__period")))
})

test_that("cast_mat_to_long_pem handles censoring and time variables correctly", {
  result <- cast_mat_to_long_pem(anls)
  df <- as.data.frame(anls@data_matrix)
  expect_true(all(result[, anls@outcome@cens_var] == 1 - result[, "psb2__event"]))
  expect_true(all(result[, anls@outcome@time_var] == result[, "psb2__tend"] - result[, "psb2__tstart"]))
})

test_that("cast_mat_to_long_pem returns correct number of rows", {
  result <- cast_mat_to_long_pem(anls)
  df <- as.data.frame(anls@data_matrix)
  cut_points <- anls@outcome@cut_points
  max_fup <- max(df[, anls@outcome@time_var])
  cut_points_keep <- cut_points[cut_points < max_fup]
  expected_rows <- nrow(df) * (length(cut_points_keep) + 1)
  expect_equal(nrow(result), expected_rows)
})
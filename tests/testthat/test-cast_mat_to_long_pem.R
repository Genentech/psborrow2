
anls <- psborrow2:::.analysis_obj(
  data_matrix = example_matrix,
  outcome = outcome_surv_pem(
    "time",
    "cnsr",
    baseline_prior = prior_normal(0, 1000),
    cut_points = c(1, 2, 3)
  ),
  borrowing = borrowing_hierarchical_commensurate("ext", prior_exponential(.001)),
  treatment = treatment_details("trt", prior_normal(0, 1000))
)

test_that("cast_mat_to_long_pem returns an analysis_obj", {
  result <- cast_mat_to_long_pem(anls)
  expect_true(inherits(result, "Analysis"))
})

test_that("cast_mat_to_long_pem handles cut points correctly", {
  anls_cut <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_surv_pem(
      "time",
      "cnsr",
      baseline_prior = prior_normal(0, 1000),
      cut_points = c(1, 2, 3, 51)
    ),
    borrowing = borrowing_hierarchical_commensurate("ext", prior_exponential(.001)),
    treatment = treatment_details("trt", prior_normal(0, 1000))
  )
  expect_warning(result <- cast_mat_to_long_pem(anls_cut), "Some cut points are greater than the maximum follow-up time")
  cut_points_keep <- anls_cut@outcome@cut_points[anls_cut@outcome@cut_points < max(anls_cut@data_matrix[, anls_cut@outcome@time_var])]
  expect_equal(length(cut_points_keep) + 1, result@outcome@n_periods)
})

test_that("cast_mat_to_long_pem handles reserved names correctly", {
  psb2__period <- rep(1, NROW(example_matrix))
  anls_nm <- psborrow2:::.analysis_obj(
    data_matrix = cbind(example_matrix, psb2__period),
    outcome = outcome_surv_pem(
      "time",
      "cnsr",
      baseline_prior = prior_normal(0, 1000),
      cut_points = c(1, 2, 3)
    ),
    borrowing = borrowing_hierarchical_commensurate("ext", prior_exponential(.001)),
    treatment = treatment_details("trt", prior_normal(0, 1000))
  )
  expect_error(cast_mat_to_long_pem(anls_nm), "Please rename your columns")
})

test_that("cast_mat_to_long_pem transforms data correctly", {
  result <- cast_mat_to_long_pem(anls)
  df <- as.data.frame(anls@data_matrix)
  long_df <- as.data.frame(result@data_matrix)
  expect_true(all(colnames(long_df) %in% c(colnames(df), "__period__")))
})

test_that("cast_mat_to_long_pem returns correct number of rows", {
  cens1 <- sum(example_matrix[, "time"] <= 1)
  cens2 <- sum(example_matrix[, "time"] <= 2 & example_matrix[, "time"] > 1)
  cens3 <- sum(example_matrix[, "time"] <= 3 & example_matrix[, "time"] > 2)

  expected_rows <- 4 * NROW(example_matrix) - 3 * cens1 - 2 * cens2 - cens3

  result <- cast_mat_to_long_pem(anls)
  long_df <- as.data.frame(result@data_matrix)
  expect_equal(nrow(long_df), expected_rows)
})
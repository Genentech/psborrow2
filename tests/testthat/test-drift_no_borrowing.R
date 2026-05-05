# Test that no-borrowing results are independent of drift
# Internal patients should be shared across drift scenarios because
# drift only affects external patients (ext=0 means ext*log(drift_hr)=0)

test_that("internal patient data is identical across drift values", {
  baseline <- create_baseline_object(
    n_trt_int = 100,
    n_ctrl_int = 100,
    n_ctrl_ext = 100
  )

  sim_obj <- create_data_simulation(
    baseline = baseline,
    event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 36)
  )

  set.seed(123)
  data <- generate(sim_obj, n = 3, treatment_hr = 0.5, drift_hr = c(1, 3))

  for (j in seq_along(data@data_list[[1]])) {
    mat_drift1 <- data@data_list[[1]][[j]]
    mat_drift3 <- data@data_list[[2]][[j]]

    int1 <- mat_drift1[mat_drift1[, "ext"] == 0, ]
    int3 <- mat_drift3[mat_drift3[, "ext"] == 0, ]

    expect_identical(int1, int3, info = paste("replicate", j))
  }
})


test_that("internal patient data is identical across drift values with covariates", {
  baseline <- create_baseline_object(
    n_trt_int = 100,
    n_ctrl_int = 100,
    n_ctrl_ext = 100,
    covariates = baseline_covariates(
      names = c("age", "score"),
      means_int = c(55, 5),
      means_ext = c(60, 5),
      covariance_int = covariance_matrix(c(5, 1)),
      covariance_ext = covariance_matrix(c(5, 1.2))
    )
  )

  sim_obj <- create_data_simulation(
    baseline = baseline,
    coefficients = c(age = 0.001, score = 0.5),
    event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 36)
  )

  set.seed(456)
  data <- generate(sim_obj, n = 2, treatment_hr = 0.5, drift_hr = c(1, 5))

  for (j in seq_along(data@data_list[[1]])) {
    mat_d1 <- data@data_list[[1]][[j]]
    mat_d5 <- data@data_list[[2]][[j]]

    int1 <- mat_d1[mat_d1[, "ext"] == 0, ]
    int5 <- mat_d5[mat_d5[, "ext"] == 0, ]

    expect_identical(int1, int5, info = paste("replicate", j))
  }
})


test_that("external patient data differs across drift values", {
  baseline <- create_baseline_object(
    n_trt_int = 100,
    n_ctrl_int = 100,
    n_ctrl_ext = 100
  )

  sim_obj <- create_data_simulation(
    baseline = baseline,
    event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 36)
  )

  set.seed(789)
  data <- generate(sim_obj, n = 1, treatment_hr = 0.5, drift_hr = c(1, 3))

  mat_d1 <- data@data_list[[1]][[1]]
  mat_d3 <- data@data_list[[2]][[1]]

  ext1 <- mat_d1[mat_d1[, "ext"] == 1, ]
  ext3 <- mat_d3[mat_d3[, "ext"] == 1, ]

  expect_false(identical(ext1[, "eventtime"], ext3[, "eventtime"]))
})


test_that("trimmed data for no-borrowing is identical across drift values", {
  baseline <- create_baseline_object(
    n_trt_int = 100,
    n_ctrl_int = 100,
    n_ctrl_ext = 100
  )

  sim_obj <- create_data_simulation(
    baseline = baseline,
    event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 36)
  )

  set.seed(321)
  data <- generate(sim_obj, n = 2, treatment_hr = 0.5, drift_hr = c(1, 3))

  for (j in seq_along(data@data_list[[1]])) {
    mat_d1 <- data@data_list[[1]][[j]]
    mat_d3 <- data@data_list[[2]][[j]]

    anls1 <- psborrow2:::.analysis_obj(
      data_matrix = mat_d1,
      outcome = outcome_surv_exponential("eventtime", "cens", prior_normal(0, 1000)),
      treatment = treatment_details("trt", prior_normal(0, 1000)),
      borrowing = borrowing_none("ext")
    )

    anls3 <- psborrow2:::.analysis_obj(
      data_matrix = mat_d3,
      outcome = outcome_surv_exponential("eventtime", "cens", prior_normal(0, 1000)),
      treatment = treatment_details("trt", prior_normal(0, 1000)),
      borrowing = borrowing_none("ext")
    )

    trim1 <- psborrow2:::trim_data_matrix(anls1)
    trim3 <- psborrow2:::trim_data_matrix(anls3)

    expect_identical(trim1, trim3, info = paste("replicate", j))
  }
})


test_that("internal data is identical across drift with multiple treatment effects", {
  baseline <- create_baseline_object(
    n_trt_int = 50,
    n_ctrl_int = 50,
    n_ctrl_ext = 50
  )

  sim_obj <- create_data_simulation(
    baseline = baseline,
    event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 36)
  )

  set.seed(999)
  data <- generate(sim_obj, n = 2, treatment_hr = c(0.5, 1.0), drift_hr = c(1, 2, 4))

  # Guide has 6 rows: 2 treatment * 3 drift, ordered by expand.grid
  # treatment_hr varies fastest: (0.5,1), (0.5,1), (0.5,1) for drift 1, 2, 4
  guide <- data@guide

  # For each treatment_hr, internal data should be identical across drift values
  for (trt in unique(guide$treatment_hr)) {
    rows <- guide[guide$treatment_hr == trt, ]
    sim_ids <- rows$sim_id

    for (j in seq_along(data@data_list[[sim_ids[1]]])) {
      ref_mat <- data@data_list[[sim_ids[1]]][[j]]
      ref_int <- ref_mat[ref_mat[, "ext"] == 0, ]

      for (k in 2:length(sim_ids)) {
        cmp_mat <- data@data_list[[sim_ids[k]]][[j]]
        cmp_int <- cmp_mat[cmp_mat[, "ext"] == 0, ]

        expect_identical(
          ref_int, cmp_int,
          info = paste("trt =", trt, "drift sim_id =", sim_ids[k], "replicate =", j)
        )
      }
    }
  }
})

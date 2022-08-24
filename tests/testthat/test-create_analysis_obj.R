# Build some valid inputs ----

ac <- add_covariates(
  c("cov1", "cov2"),
  normal_prior(0, 1000)
)
ac2 <- add_covariates(
  c("cov1", "cov2"),
  list(
    cov1 = normal_prior(0, 1000),
    cov2 = normal_prior(200, 20)
  )
)

bd_fb <- borrowing_details(
  method = "Full borrowing",
  baseline_prior = normal_prior(0, 1000),
  ext_flag_col = "ext"
)
bd_nb <- borrowing_details(
  method = "No borrowing",
  baseline_prior = normal_prior(0, 1000),
  ext_flag_col = "ext"
)
bd_db <- borrowing_details(
  method = "BDB",
  baseline_prior = normal_prior(0, 1000),
  ext_flag_col = "ext",
  tau_prior = exponential_prior(0.0001)
)

td <- treatment_details(
  "trt",
  normal_prior(0, 1000)
)

esd <- exp_surv_dist(
  time_var = "time",
  cens_var = "cnsr"
)
wpsd <- weib_ph_surv_dist(
  time_var = "time",
  cens_var = "cnsr",
  shape_prior = normal_prior(0, 1000)
)
lbo <- logistic_bin_outcome(
  binary_var = "resp"
)

test_that("Inputs classes are correct", {
  # Matrix
  expect_error(
    create_analysis_obj(
      data_matrix = as.data.frame(example_matrix),
      covariates = ac,
      outcome = esd,
      treatment = td,
      borrowing = bd_fb
    ),
    "Must be of type 'matrix', not 'data.frame'"
  )

  # Covariates
  expect_error(
    create_analysis_obj(
      data_matrix = example_matrix,
      covariates = c("cov1", "cov2"),
      outcome = esd,
      treatment = td,
      borrowing = bd_fb
    ),
    "Must inherit from class 'Covariates'/'NULL', but has class 'character'"
  )

  # Outcomes
  expect_error(
    create_analysis_obj(
      data_matrix = example_matrix,
      covariates = ac,
      outcome = exponential_prior(.001),
      treatment = td,
      borrowing = bd_fb
    ),
    "Must inherit from class 'Outcome', but has class 'ExponentialPrior'"
  )

  # Treatment
  expect_error(
    create_analysis_obj(
      data_matrix = example_matrix,
      covariates = ac,
      outcome = esd,
      treatment = "trt",
      borrowing = bd_fb
    ),
    "Must inherit from class 'Treatment', but has class 'character'"
  )

  # Borrowing
  expect_error(
    create_analysis_obj(
      data_matrix = example_matrix,
      covariates = ac,
      outcome = esd,
      treatment = td,
      borrowing = "Full borrowing"
    ),
    "Must inherit from class 'Borrowing', but has class 'character'"
  )
})

test_that("Matrix should have no missing data", {
  # Matrix should have no missing data
  example_matrix2 <- rbind(
    example_matrix,
    matrix(c(NA, 0, 1, 1, 2.2, 0, 1), ncol = 7)
  )
  expect_error(
    create_analysis_obj(
      data_matrix = example_matrix2,
      covariates = ac,
      outcome = esd,
      treatment = td,
      borrowing = bd_fb
    ),
    "Data matrix must not include any missing data"
  )
})

test_that("Columns in analysis_obj should be in matrix", {
  expect_error(
    create_analysis_obj(
      data_matrix = example_matrix,
      covariates = add_covariates(c("cov3", "cov2"),
        priors = normal_prior(0, 1000)
      ),
      outcome = esd,
      treatment = td,
      borrowing = bd_fb
    ),
    "The following specified variables were not found in `data_matrix`:\n  covariates: cov3"
  )

  expect_error(
    create_analysis_obj(
      data_matrix = example_matrix,
      covariates = ac,
      outcome = exp_surv_dist("time", "cens"),
      treatment = td,
      borrowing = bd_fb
    ),
    "The following specified variables were not found in `data_matrix`:\n  cens_var: cens"
  )

  expect_error(
    create_analysis_obj(
      data_matrix = example_matrix,
      covariates = ac,
      outcome = logistic_bin_outcome("response"),
      treatment = td,
      borrowing = bd_fb
    ),
    "The following specified variables were not found in `data_matrix`:\n  binary_var: response"
  )

  expect_error(
    create_analysis_obj(
      data_matrix = example_matrix,
      covariates = ac,
      outcome = esd,
      treatment = treatment_details(
        trt_flag_col = "treat",
        trt_prior = normal_prior(0, 100)
      ),
      borrowing = bd_fb
    ),
    "The following specified variables were not found in `data_matrix`:\n  trt_flag_col: treat"
  )

  expect_error(
    create_analysis_obj(
      data_matrix = example_matrix,
      covariates = ac,
      outcome = esd,
      treatment = td,
      borrowing = borrowing_details(
        method = "BDB",
        baseline_prior = normal_prior(0, 100),
        ext_flag_col = "tira",
        tau_prior = gamma_prior(.001, .001)
      )
    ),
    "The following specified variables were not found in `data_matrix`:\n  ext_flag_col: tira"
  )
})



test_that("All allowable inputs create Analysis object", {
  # All combinations
  for (bd in list(bd_fb, bd_nb, bd_db)) {
    for (oc in list(esd, wpsd, lbo)) {
      for (cc in list(ac, ac2, NULL)) {
        expect_class(
          create_analysis_obj(
            data_matrix = example_matrix,
            covariates = cc,
            outcome = oc,
            treatment = td,
            borrowing = bd
          ),
          "Analysis"
        )
      }
    }
  }
})

test_that("ready_to_sample flag is set", {
  result <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = ac,
    outcome = esd,
    treatment = td,
    borrowing = bd_fb
  )
  expect_false(result@ready_to_sample)
})

test_that("get_vars works for Analysis", {
  analysis <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = ac,
    outcome = esd,
    treatment = td,
    borrowing = bd_fb
  )
  expect_equal(
    get_vars(analysis),
    c(time_var = "time", cens_var = "cnsr", ext_flag_col = "ext", trt_flag_col = "trt", "cov1", "cov2")
  )

  analysis@covariates <- NULL
  expect_equal(
    get_vars(analysis),
    c(time_var = "time", cens_var = "cnsr", ext_flag_col = "ext", trt_flag_col = "trt")
  )
})

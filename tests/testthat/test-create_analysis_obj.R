# Build some valid inputs ----
mm <- structure(c(
  1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0,
  1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1,
  0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1,
  0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1,
  1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1,
  1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0,
  1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 5.32295081977934,
  6.96715560527452, 1.17501259866481, 9.45936763681621, 5.75572041253912,
  12.1139661284359, 2.64741266341488, 4.99828513121648, 5.38734198746281,
  4.74770899862051, 0.0803900761309156, 13.7720370325053, 3.03310634382069,
  10.1695853577489, 0.0720591936260462, 10.1367262049345, 2.9709762107209,
  0.659847613424063, 3.88436722227683, 3.2750634373027, 1.90838416890977,
  5.79706331825161, 4.28611800974856, 0.702194716266679, 4.74582234003252,
  6.92417557015123, 6.53942201171797, 5.88460493011677, 1.84311583921956,
  5.28505285794622, 4.34498298102206, 3.17685930818209, 11.0179639531233,
  2.14560192144267, 4.40741405311895, 10.9576044368026, 3.55944875309522,
  9.07620135719862, 1.29542022943497, 3.35630633204141, 14.1141011930051,
  14.3560852138326, 6.76962562138734, 6.60672739803918, 0.727092696356863,
  3.06457582335024, 2.27240795704226, 6.12868075434827, 7.45796004200603,
  9.23882804838511, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1,
  0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1,
  1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1,
  0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1,
  1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1,
  1
), dim = c(50L, 7L), dimnames = list(NULL, c(
  "ext", "trt", "cov1",
  "cov2", "time", "cnsr", "resp"
)))

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
      data_matrix = as.data.frame(mm),
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
      data_matrix = mm,
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
      data_matrix = mm,
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
      data_matrix = mm,
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
      data_matrix = mm,
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
  mm2 <- rbind(
    mm,
    matrix(c(NA, 0, 1, 1, 2.2, 0, 1), ncol = 7)
  )
  expect_error(
    create_analysis_obj(
      data_matrix = mm2,
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
      data_matrix = mm,
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
      data_matrix = mm,
      covariates = ac,
      outcome = exp_surv_dist("time", "cens"),
      treatment = td,
      borrowing = bd_fb
    ),
    "The following specified variables were not found in `data_matrix`:\n  cens_var: cens"
  )

  expect_error(
    create_analysis_obj(
      data_matrix = mm,
      covariates = ac,
      outcome = logistic_bin_outcome("response"),
      treatment = td,
      borrowing = bd_fb
    ),
    "The following specified variables were not found in `data_matrix`:\n  binary_var: response"
  )

  expect_error(
    create_analysis_obj(
      data_matrix = mm,
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
      data_matrix = mm,
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
            data_matrix = mm,
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
    data_matrix = mm,
    covariates = ac,
    outcome = esd,
    treatment = td,
    borrowing = bd_fb
  )
  expect_false(result@ready_to_sample)
})

test_that("get_vars works for Analysis", {
  analysis <- create_analysis_obj(
    data_matrix = mm,
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

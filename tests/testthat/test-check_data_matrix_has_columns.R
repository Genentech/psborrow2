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

anls_full <- psborrow2:::.analysis_obj(
  data_matrix = mm,
  covariates = add_covariates(c("cov1", "cov2"), normal_prior(0, 1000)),
  outcome = exp_surv_dist(
    time_var = "time",
    cens_var = "cnsr"
  ),
  treatment = treatment_details(
    "trt",
    normal_prior(0, 1000)
  ),
  borrowing = borrowing_details(
    "Full borrowing",
    ext_flag_col = "ext",
    baseline_prior = normal_prior(0, 1000)
  )
)

test_that("check_data_matrix() catches errors", {
  expect_null(psborrow2:::check_data_matrix_has_columns(anls_full))

  anls_broken <- anls_full
  anls_broken@covariates <- add_covariates(c("cov1", "cov3"), normal_prior(0, 1000))
  expect_error(
    psborrow2:::check_data_matrix_has_columns(anls_broken),
    "The following specified variables were not found in `data_matrix`:\n  covariates: cov3"
  )

  anls_broken <- anls_full
  anls_broken@treatment <- treatment_details("trt_flag", normal_prior(0, 1000))
  expect_error(
    psborrow2:::check_data_matrix_has_columns(anls_broken),
    "The following specified variables were not found in `data_matrix`:\n  treatment: trt_flag"
  )

  anls_broken <- anls_full
  anls_broken@outcome <- exp_surv_dist(time_var = "time_months", cens_var = "non_dead")
  expect_error(
    psborrow2:::check_data_matrix_has_columns(anls_broken),
    "The following specified variables were not found in `data_matrix`:\n  outcome: time_months, non_dead"
  )
})

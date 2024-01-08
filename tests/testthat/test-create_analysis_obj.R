# Tests which work without cmdstanr/CmdStan ------

# Build some valid inputs ----
ac <- add_covariates(
  c("cov1", "cov2"),
  prior_normal(0, 1000)
)
ac2 <- add_covariates(
  c("cov1", "cov2"),
  list(
    cov1 = prior_normal(0, 1000),
    cov2 = prior_normal(200, 20)
  )
)

bd_fb <- borrowing_full("ext")
bd_nb <- borrowing_none(
  ext_flag_col = "ext"
)
bd_db <- borrowing_hierarchical_commensurate(
  ext_flag_col = "ext",
  tau_prior = prior_half_cauchy(0, .0001)
)
td <- treatment_details(
  "trt",
  prior_half_normal(0, 1000)
)

esd <- outcome_surv_exponential(
  time_var = "time",
  cens_var = "cnsr",
  baseline_prior = prior_normal(0, 1000)
)
wpsd <- outcome_surv_weibull_ph(
  time_var = "time",
  cens_var = "cnsr",
  shape_prior = prior_normal(0, 1000),
  baseline_prior = prior_normal(0, 1000)
)
lbo <- outcome_bin_logistic(
  binary_var = "resp",
  baseline_prior = prior_normal(0, 1000)
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
      outcome = prior_exponential(.001),
      treatment = td,
      borrowing = bd_fb
    ),
    "Must inherit from class 'Outcome', but has class 'PriorExponential'"
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
    matrix(c(999, 0, 1, 1, NA, 1, 1, 10, 0, 1, 1), ncol = 11)
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
      covariates = add_covariates(c("cov9", "cov2"),
        priors = prior_normal(0, 1000)
      ),
      outcome = esd,
      treatment = td,
      borrowing = bd_fb
    ),
    "The following specified variables were not found in `data_matrix`:\n  covariates: cov9"
  )

  expect_error(
    create_analysis_obj(
      data_matrix = example_matrix,
      covariates = ac,
      outcome = outcome_surv_exponential("time", "cens", prior_normal(0, 1000)),
      treatment = td,
      borrowing = bd_fb
    ),
    "The following specified variables were not found in `data_matrix`:\n  cens_var: cens"
  )

  expect_error(
    create_analysis_obj(
      data_matrix = example_matrix,
      covariates = ac,
      outcome = outcome_bin_logistic("response", prior_normal(0, 1000)),
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
        trt_prior = prior_normal(0, 100)
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
      borrowing = borrowing_hierarchical_commensurate(
        ext_flag_col = "tira",
        tau_prior = prior_gamma(.001, .001)
      )
    ),
    "The following specified variables were not found in `data_matrix`:\n  ext_flag_col: tira"
  )
})


test_that("create_analysis_obj behaves gracefully if cmdstanr is unavailable", {
  skip_if(is_cmdstanr_available())

  expect_warning(
    object <- create_analysis_obj(
      data_matrix = example_matrix,
      outcome = outcome_surv_exponential(
        time_var = "time",
        cens_var = "cnsr",
        prior_normal(0, 100000)
      ),
      borrowing = borrowing_hierarchical_commensurate(
        ext_flag_col = "ext",
        tau_prior = prior_gamma(0.001, 0.001)
      ),
      treatment = treatment_details("trt", prior_normal(0, 100000))
    ),
    "could not be compiled",
    fixed = TRUE
  )

  expect_false(object@ready_to_sample)

  expect_equal(
    get_vars(object),
    c(time_var = "time", cens_var = "cnsr", ext_flag_col = "ext", trt_flag_col = "trt")
  )
})

# Tests which require cmdstanr/CmdStan ------
skip_on_cran()
skip_if_not(check_cmdstan())

# Test All combinations
borrowing_list <- list(bd_fb, bd_nb, bd_db)
outcome_list <- list(esd, wpsd, lbo)
covariates_list <- list(ac, ac2, NULL)

for (bd in 1:3) {
  for (oc in 1:3) {
    for (cc in 1:3) {
      test_that(paste(
        "All allowable inputs create Analysis object (",
        "borrowing:", bd, "outcome:", oc, "covariates:", cc, ")"
      ), {
        # Pass by position
        expect_class(
          create_analysis_obj(
            example_matrix,
            outcome_list[[oc]],
            borrowing_list[[bd]],
            td,
            covariates_list[[cc]],
            quiet = TRUE
          ),
          classes = "Analysis"
        )
      })
    }
  }
}


test_that("ready_to_sample flag is set", {
  result <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = ac,
    outcome = esd,
    treatment = td,
    borrowing = bd_fb
  )
  expect_true(result@ready_to_sample)
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

test_that("confirm the data matrix is not deep copied", {
  analysis <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = ac,
    outcome = esd,
    treatment = td,
    borrowing = bd_fb
  )
  expect_equal(
    tracemem(example_matrix),
    tracemem(analysis@data_matrix)
  )
})

rm(
  borrowing_list, outcome_list, covariates_list,
  ac, ac2,
  bd_fb, bd_nb, bd_db,
  td, esd, wpsd, lbo,
  bd, cc, oc
)

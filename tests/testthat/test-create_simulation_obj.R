skip_on_cran()
skip_if_not(check_cmdstan())

# Simulate a single matrix
sim_single_matrix <- function(true_hr = 0.6,
                              drift_hr = 1.0,
                              n = 600) {
  # Create a data frame with the subject IDs and treatment covariate
  cov <- data.frame(
    id = 1:n,
    trt = rbinom(n, 1, 0.5),
    cov1 = rbinom(n, 1, 0.5)
  )
  cov$ext <- ifelse(cov$trt == 1L, 0L, rbinom(sum(cov$trt), 1, 0.5))

  # Simulate the event times
  dat <- simsurv::simsurv(
    lambdas = 0.1,
    gammas = 1.5,
    betas = c(
      trt = log(true_hr),
      ext = log(drift_hr),
      cov1 = log(0.8)
    ),
    x = cov,
    maxt = 5
  )

  dat$censor <- 1 - dat$status

  # Merge the simulated event times onto covariate data frame
  dat <- merge(cov, dat)

  as.matrix(dat)
}

# Valid data list
data_matrix_list <- list(
  list(sim_single_matrix(true_hr = 0.6), sim_single_matrix(true_hr = 0.6)),
  list(sim_single_matrix(true_hr = 0.8), sim_single_matrix(true_hr = 0.8))
)
valid_data_list <- sim_data_list(
  data_matrix_list,
  data.frame(true_hr = c(0.6, 0.8), drift_hr = c(1.0, 1.0), index = 1:2),
  effect = "true_hr",
  drift = "drift_hr",
  index = "index"
)

# Valid borrowing list
valid_borrowing <- sim_borrowing_list(
  list(
    bdb = borrowing_hierarchical_commensurate(ext_flag_col = "ext", tau_prior = prior_exponential(0.0001)),
    full = borrowing_full("ext")
  )
)

# Valid outcome list
valid_outcome <- sim_outcome_list(
  list(standard_outcome = outcome_surv_exponential(
    time_var = "eventtime",
    cens_var = "censor",
    baseline_prior = prior_normal(0, 1000)
  ))
)

# Valid covariate list
valid_covariate <- sim_covariate_list(
  list(
    cov1 = add_covariates("cov1", prior_normal(0, 1000)),
    `no covs` = NULL
  )
)

# Valid treatment list
valid_treatment <- sim_treatment_list(
  list(standard_tx = treatment_details(trt_flag_col = "trt", trt_prior = prior_normal(0, 1000)))
)

test_that("`create_simulation_obj()` input classes are correct", {
  expect_error(
    create_simulation_obj(
      data_matrix_list = data_matrix_list,
      covariate = valid_covariate,
      outcome = valid_outcome,
      borrowing = valid_borrowing,
      treatment = valid_treatment
    ),
    "Must inherit from class 'SimDataList'"
  )

  expect_error(
    create_simulation_obj(
      data_matrix_list = valid_data_list,
      covariate = c("cov1"),
      outcome = valid_outcome,
      borrowing = valid_borrowing,
      treatment = valid_treatment
    ),
    "Must inherit from class 'SimCovariateList'"
  )

  expect_error(
    create_simulation_obj(
      data_matrix_list = valid_data_list,
      outcome = list(outcome_surv_exponential("eventtime", "censor", prior_normal(0, 1000))),
      borrowing = valid_borrowing,
      treatment = valid_treatment
    ),
    "Must inherit from class 'SimOutcomeList'"
  )

  expect_error(
    create_simulation_obj(
      data_matrix_list = valid_data_list,
      outcome = valid_outcome,
      borrowing = list(borrowing_full("ext")),
      treatment = valid_treatment
    ),
    "Must inherit from class 'SimBorrowingList'"
  )

  expect_error(
    create_simulation_obj(
      data_matrix_list = valid_data_list,
      outcome = valid_outcome,
      borrowing = valid_borrowing,
      treatment = list(treatment = treatment_details(
        "trt",
        trt_prior = prior_normal(0, 1000)
      ))
    ),
    "Must inherit from class 'SimTreatmentList'"
  )
})

test_that("`create_simulation_obj()` correct inputs create `Simulation` object", {
  expect_class(
    create_simulation_obj(
      data_matrix_list = valid_data_list,
      covariate = valid_covariate,
      outcome = valid_outcome,
      borrowing = valid_borrowing,
      treatment = valid_treatment
    ),
    "Simulation"
  )

  expect_class(
    create_simulation_obj(
      data_matrix_list = valid_data_list,
      outcome = valid_outcome,
      borrowing = valid_borrowing,
      treatment = valid_treatment
    ),
    "Simulation"
  )

  expect_class(
    create_simulation_obj(
      data_matrix_list = valid_data_list,
      covariate = add_covariates(c("cov1"), prior_normal(0, 100)),
      outcome = valid_outcome,
      borrowing = valid_borrowing,
      treatment = valid_treatment
    ),
    "Simulation"
  )

  expect_class(
    create_simulation_obj(
      data_matrix_list = valid_data_list,
      covariate = valid_covariate,
      outcome = outcome_surv_exponential("eventtime", "censor", prior_normal(0, 100)),
      borrowing = valid_borrowing,
      treatment = valid_treatment
    ),
    "Simulation"
  )

  expect_class(
    create_simulation_obj(
      data_matrix_list = valid_data_list,
      covariate = valid_covariate,
      outcome = valid_outcome,
      borrowing = borrowing_full("ext"),
      treatment = valid_treatment
    ),
    "Simulation"
  )

  expect_class(
    create_simulation_obj(
      data_matrix_list = valid_data_list,
      covariate = valid_covariate,
      outcome = valid_outcome,
      borrowing = valid_borrowing,
      treatment = treatment_details("trt", prior_normal(0, 100))
    ),
    "Simulation"
  )
})

test_that("`create_simulation_obj()` catches incorrect column names", {
  expect_error(
    create_simulation_obj(
      data_matrix_list = valid_data_list,
      covariate = sim_covariate_list(
        list(basic = add_covariates("wrong_cov",
          priors = prior_normal(0, 1000)
        ))
      ),
      outcome = valid_outcome,
      borrowing = valid_borrowing,
      treatment = valid_treatment
    ),
    "missing in some simulated data matrices: 'wrong_cov'"
  )

  expect_error(
    create_simulation_obj(
      data_matrix_list = valid_data_list,
      outcome = sim_outcome_list(
        list(basic = outcome_surv_exponential(
          "eventtime",
          "wrong_censor",
          prior_normal(0, 1000)
        ))
      ),
      borrowing = valid_borrowing,
      treatment = valid_treatment
    ),
    "missing in some simulated data matrices: 'wrong_censor'"
  )

  expect_error(
    create_simulation_obj(
      data_matrix_list = valid_data_list,
      outcome = valid_outcome,
      borrowing = sim_borrowing_list(
        list(normal = borrowing_none(
          "wrong_ext"
        ))
      ),
      treatment = valid_treatment
    ),
    "missing in some simulated data matrices: 'wrong_ext'"
  )

  expect_error(
    create_simulation_obj(
      data_matrix_list = valid_data_list,
      outcome = valid_outcome,
      borrowing = valid_borrowing,
      treatment = sim_treatment_list(
        list(treatment = treatment_details(
          "wrong_trt",
          trt_prior = prior_normal(0, 1000)
        ))
      )
    ),
    "missing in some simulated data matrices: 'wrong_trt'"
  )
})

test_that("`create_simulation_obj()` catches missing data", {
  data_list_missing <- data_matrix_list
  data_list_missing[[2]][[1]][22, "eventtime"] <- NA_real_

  expect_error(
    create_simulation_obj(
      data_matrix_list = sim_data_list(data_list_missing,
        data.frame(true_hr = c(0.6, 0.8), drift_hr = c(1.0, 1.0), index = 1:2),
        effect = "true_hr",
        drift = "drift_hr",
        index = "index"
      ),
      covariate = valid_covariate,
      outcome = valid_outcome,
      borrowing = valid_borrowing,
      treatment = valid_treatment
    ),
    "Missing data detected in >1 matrix in `data_matrix_list`"
  )
})

test_that("`create_simulation_obj()` correctly gets number of combinations", {
  valid_simulation_obj8 <- create_simulation_obj(
    data_matrix_list = valid_data_list,
    covariate = valid_covariate,
    outcome = valid_outcome,
    borrowing = valid_borrowing,
    treatment = valid_treatment
  )

  valid_simulation_obj16 <- create_simulation_obj(
    data_matrix_list = valid_data_list,
    covariate = valid_covariate,
    outcome = sim_outcome_list(list(
      exp = outcome_surv_exponential(
        time_var = "eventtime",
        cens_var = "censor",
        prior_normal(0, 1000)
      ),
      weib = outcome_surv_weibull_ph(
        time_var = "eventtime",
        cens_var = "censor",
        prior_normal(0, 1000),
        prior_normal(0, 1000)
      )
    )),
    borrowing = valid_borrowing,
    treatment = valid_treatment
  )

  expect_equal(NROW(valid_simulation_obj8@guide), 8)
  expect_equal(valid_simulation_obj8@n_combos, 8)
  expect_equal(sum(valid_simulation_obj8@guide$n_datasets_per_param), 16)
  expect_equal(valid_simulation_obj8@n_analyses, 16)
  expect_equal(NROW(valid_simulation_obj16@guide), 16)
  expect_equal(valid_simulation_obj16@n_combos, 16)
  expect_equal(sum(valid_simulation_obj16@guide$n_datasets_per_param), 32)
  expect_equal(valid_simulation_obj16@n_analyses, 32)
})

test_that("`create_simulation_obj()` correctly creates analysis objects", {
  valid_simulation_obj <- create_simulation_obj(
    data_matrix_list = valid_data_list,
    covariate = valid_covariate,
    outcome = valid_outcome,
    borrowing = valid_borrowing,
    treatment = valid_treatment
  )

  expect_true(
    all(
      vapply(
        valid_simulation_obj@analysis_obj_list,
        FUN = function(obj) {
          all(
            vapply(obj,
              function(obj2) {
                is(obj2, "Analysis")
              },
              FUN.VALUE = logical(1)
            )
          )
        },
        FUN.VALUE = logical(1)
      )
    )
  )
})

test_that("`create_simulation_obj()` does not create deep copies", {
  so <- create_simulation_obj(
    data_matrix_list = valid_data_list,
    covariate = valid_covariate,
    outcome = valid_outcome,
    borrowing = valid_borrowing,
    treatment = valid_treatment
  )

  for (i in 1:so@n_combos) {
    analysis_list <- so@analysis_obj_list[[i]]
    data_matrix_list <- so@data_matrix_list@data_list[[
      so@guide[[so@data_matrix_list@index]][i]
    ]]
    expect_equal(
      NROW(analysis_list),
      NROW(data_matrix_list)
    )
    for (j in seq_along(analysis_list)) {
      expect_equal(
        tracemem(analysis_list[[j]]@data_matrix),
        tracemem(data_matrix_list[[j]])
      )
    }
  }
})

test_that("`show_guide()` gives correct output", {

  valid_simulation_obj <- create_simulation_obj(
    data_matrix_list = valid_data_list,
    covariate = valid_covariate,
    outcome = valid_outcome,
    borrowing = valid_borrowing,
    treatment = valid_treatment
  )

  guide <- show_guide(valid_simulation_obj)

  expect_class(
    guide,
    "data.frame"
  )

  expect_equal(
    NROW(guide),
    8
  )

  expect_equal(
    NCOL(guide),
    8
  )

  expect_equal(
    1,
    NROW(guide[
      guide$true_hr == 0.6 &
      guide$drift_hr == 1.0 & 
      guide$borrowing_scenario == "bdb" & 
      guide$covariate_scenario == "cov1" &
      guide$treatment_scenario == "standard_tx",
    ])
  )
})
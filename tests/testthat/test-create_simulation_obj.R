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
data_list <- list(
  list(sim_single_matrix(true_hr = 0.6), sim_single_matrix(true_hr = 0.6)),
  list(sim_single_matrix(true_hr = 0.8), sim_single_matrix(true_hr = 0.8))
)
valid_data_list <- sim_data_list(
  data_list,
  data.frame(true_hr = c(0.6, 0.8), drift_hr = c(1.0, 1.0)),
  effect = "true_hr",
  drift = "drift_hr"
)

# Valid borrowing list
valid_borrowing_list <- sim_borrowing_list(
  list(bdb = borrowing_details(method = "BDB",ext_flag_col = "ext", tau_prior = exponential_prior(0.0001)),
       full = borrowing_details(method = "Full borrowing", ext_flag_col = "ext"))
)

# Valid outcome list
valid_outcome_list <- sim_outcome_list(
  list(standard_outcome = exp_surv_dist(time_var = "eventtime", cens_var = "censor", baseline_prior = normal_prior(0, 1000)))
)

# Valid covariate list
valid_covariate_list = sim_covariate_list(
  list(cov1 = add_covariates("cov1", normal_prior(0, 1000)),
       `no covs` = NULL)
)

# Valid treatment list
valid_treatment_list = sim_treatment_list(
  list(standard_tx = treatment_details(trt_flag_col = "trt", trt_prior = normal_prior(0, 1000)))
)

test_that("`create_simulation_obj()` input classes are correct", {

  expect_error(
    create_simulation_obj(data_list = data_list,
                          covariate_list = valid_covariate_list,
                          outcome_list = valid_outcome_list,
                          borrowing_list = valid_borrowing_list,
                          treatment_list = valid_treatment_list),
    "Must inherit from class 'SimDataList'"
  )

  expect_error(
    create_simulation_obj(data_list = valid_data_list,
                          covariate_list = c("cov1"),
                          outcome_list = valid_outcome_list,
                          borrowing_list = valid_borrowing_list,
                          treatment_list = valid_treatment_list),
    "Must inherit from class 'SimCovariateList'"
  )

  expect_error(
    create_simulation_obj(data_list = valid_data_list,
                          outcome_list = exp_surv_dist("eventtime", "censor", normal_prior(0, 1000)),
                          borrowing_list = valid_borrowing_list,
                          treatment_list = valid_treatment_list),
    "Must inherit from class 'SimOutcomeList'"
  )

  expect_error(
    create_simulation_obj(data_list = valid_data_list,
                          outcome_list = valid_outcome_list,
                          borrowing_list = borrowing_details("Full borrowing", "ext"),
                          treatment_list = valid_treatment_list),
    "Must inherit from class 'SimBorrowingList'"
  )

  expect_error(
    create_simulation_obj(data_list = valid_data_list,
                          outcome_list = valid_outcome_list,
                          borrowing_list = valid_borrowing_list,
                          treatment_list = list(treatment = treatment_details("trt", trt_prior = normal_prior(0, 1000)))),
    "Must inherit from class 'SimTreatmentList'"
  )
})

test_that("`create_simulation_obj()` correct inputs create `Simulation` object", {

  expect_class(
    create_simulation_obj(data_list = valid_data_list,
                          covariate_list = valid_covariate_list,
                          outcome_list = valid_outcome_list,
                          borrowing_list = valid_borrowing_list,
                          treatment_list = valid_treatment_list),
    "Simulation"
  )

  expect_class(
    create_simulation_obj(data_list = valid_data_list,
                          outcome_list = valid_outcome_list,
                          borrowing_list = valid_borrowing_list,
                          treatment_list = valid_treatment_list),
    "Simulation"
  )


})

test_that("`create_simulation_obj()` catches incorrect column names", {

  expect_error(
    create_simulation_obj(data_list = valid_data_list,
                          covariate_list = sim_covariate_list(list(basic = add_covariates("wrong_cov", priors = normal_prior(0, 1000)))),
                          outcome_list = valid_outcome_list,
                          borrowing_list = valid_borrowing_list,
                          treatment_list = valid_treatment_list),
    "missing in some simulated data matrices: 'wrong_cov'"
  )

  expect_error(
    create_simulation_obj(data_list = valid_data_list,
                          outcome_list = sim_outcome_list(list(basic = exp_surv_dist("eventtime", "wrong_censor", normal_prior(0, 1000)))),
                          borrowing_list = valid_borrowing_list,
                          treatment_list = valid_treatment_list),
    "missing in some simulated data matrices: 'wrong_censor'"
  )

  expect_error(
    create_simulation_obj(data_list = valid_data_list,
                          outcome_list = valid_outcome_list,
                          borrowing_list = sim_borrowing_list(list(normal = borrowing_details("Full borrowing", "wrong_ext"))),
                          treatment_list = valid_treatment_list),
    "missing in some simulated data matrices: 'wrong_ext'"
  )

  expect_error(
    create_simulation_obj(data_list = valid_data_list,
                          outcome_list = valid_outcome_list,
                          borrowing_list = valid_borrowing_list,
                          treatment_list = sim_treatment_list(list(treatment = treatment_details("wrong_trt", trt_prior = normal_prior(0, 1000))))),
    "missing in some simulated data matrices: 'wrong_trt'"
  )
})

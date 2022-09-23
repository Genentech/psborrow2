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
    bdb = borrowing_details(method = "BDB", ext_flag_col = "ext", tau_prior = exponential_prior(0.0001)),
    full = borrowing_details(method = "Full borrowing", ext_flag_col = "ext")
  )
)

# Valid outcome list
valid_outcome <- sim_outcome_list(
  list(standard_outcome = exp_surv_dist(
    time_var = "eventtime",
    cens_var = "censor",
    baseline_prior = normal_prior(0, 1000)
  ))
)

# Valid covariate list
valid_covariate <- sim_covariate_list(
  list(
    cov1 = add_covariates("cov1", normal_prior(0, 1000)),
    `no covs` = NULL
  )
)

# Valid treatment list
valid_treatment <- sim_treatment_list(
  list(standard_tx = treatment_details(trt_flag_col = "trt", trt_prior = normal_prior(0, 1000)))
)

# Valid simulation object
valid_sim_obj <- create_simulation_obj(
  data_matrix_list = valid_data_list,
  outcome = valid_outcome,
  borrowing = valid_borrowing,
  treatment = valid_treatment
)

test_that("mcmc_sample.Simulation() creates an object of class `MCMCSimulationResult", {
  skip_on_cran()
  skip_on_ci()
  mcmc_res <- mcmc_sample(
    valid_sim_obj,
    keep_draws = TRUE,
    chains = 1,
    iter_sampling = 1000
  )
  result_df <- get_results(mcmc_res)
  mcmc_model_results <- get_cmd_stan_models(mcmc_res)

  expect_class(mcmc_res, "MCMCSimulationResult")
  expect_class(result_df, "data.frame")
  expect_class(mcmc_model_results, "list")
  expect_class(mcmc_model_results[[1]], "list")
  expect_class(mcmc_model_results[[1]][[1]], "CmdStanModel")
  expect_equal(sum(is.na(result_df$coverage)), 0)
})
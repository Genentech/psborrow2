devtools::load_all()
library(simsurv)
sim_single_matrix <- function(true_hr = 0.6,
                              drift_hr = 1.0,
                              n = 600) {
  cov <- data.frame(
    id = 1:n,
    trt = rbinom(n, 1, 0.5)
  )
  cov$ext <- ifelse(cov$trt == 1L, 0L, rbinom(sum(cov$trt), 1, 0.5))

  # Simulate the event times
  dat <- simsurv(
    lambdas = 0.1,
    gammas = 1.5,
    betas = c(
      trt = log(true_hr),
      ext = log(drift_hr)
    ),
    x = cov,
    maxt = 5
  )

  dat$censor <- 1 - dat$status

  # Merge the simulated event times onto covariate data frame
  dat <- merge(cov, dat)

  as.matrix(dat)
}

set.seed(123)

n <- 20

# Create list of lists of data
my_data_list <- list(
  replicate(n, sim_single_matrix(true_hr = 0.6, drift_hr = 1.0), simplify = FALSE),
  replicate(n, sim_single_matrix(true_hr = 1.0, drift_hr = 1.0), simplify = FALSE),
  replicate(n, sim_single_matrix(true_hr = 0.6, drift_hr = 1.5), simplify = FALSE),
  replicate(n, sim_single_matrix(true_hr = 1.0, drift_hr = 1.5), simplify = FALSE)
)

my_sim_data_guide <- expand.grid(
  true_hr = c(0.6, 1.0),
  drift_hr = c(1.0, 1.5)
)

my_sim_data_guide$id <- seq(1, NROW(my_sim_data_guide))

my_sim_data_list <- sim_data_list(
  data_list = my_data_list,
  guide = my_sim_data_guide,
  effect = "true_hr",
  drift = "drift_hr",
  index = "id"
)

my_borrowing_list <- sim_borrowing_list(
  list(
    "No borrowing" = borrowing_details("No borrowing", "ext"),
    "Full borrowing" = borrowing_details("Full borrowing", "ext"),
    "BDB - conservative" = borrowing_details("BDB", "ext", gamma_prior(0.001, 0.001)),
    "BDB - aggressive" = borrowing_details("BDB", "ext", gamma_prior(1, 0.001))
  )
)

simulation_obj <- create_simulation_obj(
  my_sim_data_list,
  outcome = exp_surv_dist("eventtime",
    "censor",
    baseline_prior = normal_prior(0, 10000)
  ),
  borrowing = my_borrowing_list,
  treatment = treatment_details(
    trt_flag_col = "trt",
    trt_prior = normal_prior(0, 10000)
  )
)

simulation_res <- mcmc_sample(
  x = simulation_obj,
  iter_warmup = 500,
  iter_sampling = 250,
  chains = 1
)


ggplot(simulation_res_df) +
  geom_bar(aes(x = factor(true_hr), fill = borrowing_scenario, y = mse_mean),
    stat = "identity", position = "dodge"
  ) +
  labs(
    fill = "Borrowing scenario",
    x = "True HR",
    y = "MSE"
  ) +
  facet_wrap(~ paste0("drift HR = ", drift_hr)) +
  scale_fill_manual(values = c("#29339B", "#74A4BC", "#B6D6CC"))


ggplot(simulation_res_df[simulation_res_df$true_hr == 1.0, ]) +
  geom_bar(aes(x = factor(drift_hr), fill = borrowing_scenario, y = 1 - true_coverage),
    stat = "identity", position = "dodge"
  ) +
  labs(
    fill = "Borrowing scenario",
    x = "drift HR",
    y = "Type I error"
  ) +
  scale_fill_manual(values = c("#29339B", "#74A4BC", "#B6D6CC"))


ggplot(simulation_res_df[simulation_res_df$true_hr == 0.6, ]) +
  geom_bar(aes(x = factor(drift_hr), fill = borrowing_scenario, y = 1 - null_coverage),
    stat = "identity", position = "dodge"
  ) +
  labs(
    fill = "Borrowing scenario",
    x = "drift HR",
    y = "Type I error"
  ) +
  scale_fill_manual(values = c("#29339B", "#74A4BC", "#B6D6CC"))

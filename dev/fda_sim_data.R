devtools::load_all()
library(simsurv)
source("./dev/sim_data.R")

set.seed(123)

n <- 50

# Create list of lists of data
my_data_list <- list(
  replicate(n, sim_single_matrix(
    n = 300, hr = 0.6, inherent_drift_hr = 1.0,
    cov1_hr = 1.0,
    cov2_hr = 1.0,
    cov3_hr = 1.0,
    cov4_hr = 1.0
  ), simplify = FALSE),
  replicate(n, sim_single_matrix(
    n = 250, hr = 1.0, inherent_drift_hr = 1.0,
    cov1_hr = 1.0,
    cov2_hr = 1.0,
    cov3_hr = 1.0,
    cov4_hr = 1.0
  ), simplify = FALSE),
  replicate(n, sim_single_matrix(
    n = 250, hr = 0.6, inherent_drift_hr = 1.5,
    cov1_hr = 1.0,
    cov2_hr = 1.0,
    cov3_hr = 1.0,
    cov4_hr = 1.0
  ), simplify = FALSE),
  replicate(n, sim_single_matrix(
    n = 250, hr = 1.0, inherent_drift_hr = 1.5,
    cov1_hr = 1.0,
    cov2_hr = 1.0,
    cov3_hr = 1.0,
    cov4_hr = 1.0
  ), simplify = FALSE)
)

my_sim_data_guide <- expand.grid(
  true_hr = c(0.6, 1.0),
  drift_hr = c("No drift HR", "Moderate drift HR")
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
  outcome = exp_surv_dist("time",
    "cnsr",
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

simulation_res_df <- get_results(simulation_res)
simulation_res_df$borrowing_scenario <- factor(simulation_res_df$borrowing_scenario,
  levels = c(
    "No borrowing",
    "BDB - conservative",
    "BDB - aggressive",
    "Full borrowing"
  )
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
  facet_wrap(~drift_hr) +
  scale_fill_manual(values = c("#EF798A", "#F7A9A8", "#7D82B8", "#613F75"))


ggplot(simulation_res_df[simulation_res_df$true_hr == 1.0, ]) +
  geom_bar(aes(x = factor(drift_hr), fill = borrowing_scenario, y = 1 - true_coverage),
    stat = "identity", position = "dodge"
  ) +
  labs(
    fill = "Borrowing scenario",
    x = "drift HR",
    y = "Type I error"
  ) +
  scale_fill_manual(values = c("#EF798A", "#F7A9A8", "#7D82B8", "#613F75")) +
  scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0,1))+
  geom_hline(aes(yintercept = 0.05), linetype = 2)


ggplot(simulation_res_df[simulation_res_df$true_hr == 0.6, ]) +
  geom_bar(aes(x = factor(drift_hr), fill = borrowing_scenario, y = 1 - null_coverage),
    stat = "identity", position = "dodge"
  ) +
  labs(
    fill = "Borrowing scenario",
    x = "drift HR",
    y = "Power"
  ) +
  scale_fill_manual(values = c("#EF798A", "#F7A9A8", "#7D82B8", "#613F75"))+
  scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0,1)) +
  geom_hline(aes(yintercept = 0.80), linetype = 2)

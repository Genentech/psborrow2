############################################################
#                                                          #
#         OVERVIEW OF BDB SIMULATION IN PSBORROW2          #
#                  Matthew Secrest                         #
#                   October 2022                           #
#                                                          #
############################################################

# Goal of this demo is to:
# Simulate the impact of a BDB study on trial design

# Load dependancies----

# psborrow2
library(psborrow2)

# survival analysis
library(survival)
library(survminer)

# plotting
library(ggplot2)

# simulating survival data
library(simsurv)

# summarizing results
library(broom)

############################################################
# The end goal ----
############################################################
# ?create_simulation_obj

############################################################
# Simulate data ----
############################################################

# DSB: I would put even more comments in this script since it is more complex
# than the analysis script - that will help folks to go back later and understand
# the details.

## Create function to simulate single matrix
make_odds <- function(prob) {
  prob / (1 - prob)
}

make_prob <- function(odds) {
  odds / (1 + odds)
}

sim_single_matrix <- function(n = 500,
                              prob = c(0.1, 0.2, 0.7),
                              hr = 0.70,
                              inherent_drift_hr = 1.0,
                              cov1_baseline_prob = 0.5,
                              cov2_baseline_prob = 0.25,
                              cov3_baseline_prob = 0.75,
                              cov4_baseline_prob = 0.5,
                              cov1_hr = 1.0,
                              cov2_hr = 1.0,
                              cov3_hr = 1.0,
                              cov4_hr = 1.0,
                              ext_or_cov1 = 3,
                              ext_or_cov2 = 3,
                              ext_or_cov3 = 0.25,
                              ext_or_cov4 = 0.25,
                              trt_or_cov1 = 1.5,
                              trt_or_cov2 = 1.5,
                              trt_or_cov3 = 0.75,
                              trt_or_cov4 = 0.75) {
  #' Creates simulated data for survival and binary endpoints w/ 4 covariates
  #'
  #' @param n number of simulated patients
  #' @param prob numeric vector of probabilities for internal control,
  #' external control, internal experimental, in that order.
  #' @param hr true HR
  #' @param inherent_drift_hr the baseline HR between internal
  #' and external controls not caused by covariates
  #' @param cov1_baseline_prob probability of cov1 in ext = 0 groups
  #' @param cov2_baseline_prob probability of cov2 in ext = 0 groups
  #' @param cov3_baseline_prob probability of cov3 in ext = 0 groups
  #' @param cov4_baseline_prob probability of cov4 in ext = 0 groups
  #' @param cov1_hr true HR for cov1
  #' @param cov2_hr true HR for cov2
  #' @param cov3_hr true HR for cov3
  #' @param cov4_hr true HR for cov4
  #' @param ext_or_cov1 OR of being cov1 for external controls
  #' @param ext_or_cov2 OR of being cov2 for external controls
  #' @param ext_or_cov3 OR of being cov3 for external controls
  #' @param ext_or_cov4 OR of being cov4 for external controls

  # checks
  if (sum(prob) != 1.0) {
    stop("prob must sum to 1")
  }

  # Depend
  require(simsurv)
  require(broom)
  require(survival)

  # Create a data frame with the subject IDs and treatment group
  cov <- data.frame(
    id = 1:n,
    ext = c(
      rep(0L, n * (prob[1] + prob[2])),
      rep(1L, n * prob[3])
    ),
    trt = c(
      rep(0L, n * prob[1]),
      rep(1L, n * prob[2]),
      rep(0L, n * prob[3])
    )
  )
  cov$cov1 <- cov$cov2 <- cov$cov3 <- cov$cov4 <- integer(length = nrow(cov))

  for (i in seq(1, NROW(cov))) {
    if (cov$ext[i] == 1) {
      cov$cov1[i] <- rbinom(1, 1, make_prob(ext_or_cov1 * make_odds(cov1_baseline_prob)))
      cov$cov2[i] <- rbinom(1, 1, make_prob(ext_or_cov2 * make_odds(cov2_baseline_prob)))
      cov$cov3[i] <- rbinom(1, 1, make_prob(ext_or_cov3 * make_odds(cov3_baseline_prob)))
      cov$cov4[i] <- rbinom(1, 1, make_prob(ext_or_cov4 * make_odds(cov4_baseline_prob)))
    } else if (cov$trt[i] == 1) {
      cov$cov1[i] <- rbinom(1, 1, make_prob(trt_or_cov1 * make_odds(cov1_baseline_prob)))
      cov$cov2[i] <- rbinom(1, 1, make_prob(trt_or_cov2 * make_odds(cov2_baseline_prob)))
      cov$cov3[i] <- rbinom(1, 1, make_prob(trt_or_cov3 * make_odds(cov3_baseline_prob)))
      cov$cov4[i] <- rbinom(1, 1, make_prob(trt_or_cov4 * make_odds(cov4_baseline_prob)))
    } else {
      cov$cov1[i] <- rbinom(1, 1, cov1_baseline_prob)
      cov$cov2[i] <- rbinom(1, 1, cov2_baseline_prob)
      cov$cov3[i] <- rbinom(1, 1, cov3_baseline_prob)
      cov$cov4[i] <- rbinom(1, 1, cov4_baseline_prob)
    }
  }

  # Simulate the event times
  dat <- simsurv(
    lambdas = 0.1,
    dist = "exponential",
    betas = c(
      trt = log(hr),
      ext = log(inherent_drift_hr),
      cov1 = log(cov1_hr),
      cov2 = log(cov2_hr),
      cov3 = log(cov3_hr),
      cov4 = log(cov4_hr)
    ),
    x = cov,
    maxt = 50
  )

  dat$censor <- 1 - dat$status

  # Merge the simulated event times onto covariate data frame
  dat <- merge(cov, dat)

  dat <- dat[, c(
    "id", "ext", "trt", "cov4", "cov3", "cov2", "cov1",
    "eventtime", "status", "censor"
  )]

  colnames(dat) <- c(
    "id", "ext", "trt", "cov4", "cov3", "cov2", "cov1",
    "time", "status", "cnsr"
  )

  as.matrix(dat)
}

sim_single_matrix()

# Set seed
set.seed(123)

# Set number of simulations per scenario
n <- 50

# Create list of lists of data
my_data_list <- list(
  replicate(n,
    sim_single_matrix(n = 250, hr = 0.6, inherent_drift_hr = 1.0),
    simplify = FALSE
  ),
  replicate(n,
    sim_single_matrix(n = 250, hr = 1.0, inherent_drift_hr = 1.0),
    simplify = FALSE
  ),
  replicate(n,
    sim_single_matrix(n = 250, hr = 0.6, inherent_drift_hr = 1.5),
    simplify = FALSE
  ),
  replicate(n,
    sim_single_matrix(n = 250, hr = 1.0, inherent_drift_hr = 1.5),
    simplify = FALSE
  )
)

NROW(my_data_list)
NROW(my_data_list[[1]])
head(my_data_list[[1]][[1]])

############################################################
# Create data list object ----
############################################################
# ?sim_data_list

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

############################################################
# Create borrowing list object ----
############################################################
# ?sim_borrowing_list

my_borrowing_list <- sim_borrowing_list(
  list(
    "No borrowing" = borrowing_details("No borrowing", "ext"),
    "Full borrowing" = borrowing_details("Full borrowing", "ext"),
    "BDB - conservative" = borrowing_details("BDB", "ext", gamma_prior(0.001, 0.001)),
    "BDB - aggressive" = borrowing_details("BDB", "ext", gamma_prior(1, 0.001))
  )
)

############################################################
# Simulation study analysis----
############################################################

## The end goal
# ?create_simulation_obj

simulation_obj <- create_simulation_obj(
  my_sim_data_list,
  outcome = exp_surv_dist("time", "cnsr", baseline_prior = normal_prior(0, 10000)),
  borrowing = my_borrowing_list,
  treatment = treatment_details(trt_flag_col = "trt", trt_prior = normal_prior(0, 10000))
)
simulation_obj

simulation_res <- mcmc_sample(
  x = simulation_obj,
  iter_warmup = 500,
  iter_sampling = 250,
  chains = 1
)

simulation_res_df <- get_results(simulation_res)

############################################################
# Plot results----
############################################################

# factorize borrowing scenario
simulation_res_df$borrowing_scenario <- factor(simulation_res_df$borrowing_scenario,
  levels = c(
    "No borrowing",
    "BDB - conservative",
    "BDB - aggressive",
    "Full borrowing"
  )
)
## Power ----
ggplot(simulation_res_df[simulation_res_df$true_hr == 0.6, ]) +
  geom_bar(aes(x = factor(drift_hr), fill = borrowing_scenario, y = 1 - null_coverage),
    stat = "identity", position = "dodge"
  ) +
  labs(
    fill = "Borrowing scenario",
    x = "drift HR",
    y = "Power"
  ) +
  scale_fill_manual(values = c("#EF798A", "#F7A9A8", "#7D82B8", "#613F75")) +
  scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0, 1)) +
  geom_hline(aes(yintercept = 0.80), linetype = 2)

## Type I error ----
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
  scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0, 0.8)) +
  geom_hline(aes(yintercept = 0.05), linetype = 2)

## MSE ----
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

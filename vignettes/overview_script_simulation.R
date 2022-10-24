############################################################
#                                                          #
#         OVERVIEW OF BDB SIMULATION IN PSBORROW2          #
#                  Matthew Secrest                         #
#                   October 2022                           #
#                                                          #
############################################################

# The goal of this demo is to: simulate the impact of
# a BDB study on trial design

# load dependancies----
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
# Simulation studies----
############################################################
# Our goal is to create a simulation object which will
# help facilicate a simulation study.

# We do this with:
?create_simulation_obj

# Note: the first argument is an object of type SimDataList.
# We'll come back to this after writing a function to
# simulate data below.

############################################################
# Create a function to simulate a single matrix----
############################################################

# While psborrow2 can be used to conduct simulation studies,
# it does not currently perform any data generation tasks
# on behalf of the user. Therefore, we need to create a function
# that will simulate data for a single matrix.

# function to create a single matrix
sim_single_matrix <- function(n = 500, # n simulated pts
                              prob = c(
                                0.1, # proportion internal control
                                0.2, # proportion internal treated
                                0.7
                              ), # proportion external control
                              hr = 0.70, # true HR for the treatment
                              inherent_drift_hr = 1.0 # HR of external/internal
) {
  # checks
  if (sum(prob) != 1.0) {
    stop("prob must sum to 1")
  }

  # data frame with the subject IDs and treatment group
  df_ids <- data.frame(
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

  # simulated event times
  df_surv <- simsurv(
    lambdas = 0.1,
    dist = "exponential",
    betas = c(
      trt = log(hr),
      ext = log(inherent_drift_hr)
    ),
    x = df_ids,
    maxt = 50
  )

  df_surv$censor <- 1 - df_surv$status

  # merge the simulated event times into data frame
  df <- merge(df_ids, df_surv)
  df <- df[, c("id", "ext", "trt", "eventtime", "status", "censor")]
  colnames(df) <- c("id", "ext", "trt", "time", "status", "cnsr")
  return(as.matrix(df))
}

# confirm we get a single matrix
sim_single_matrix()

############################################################
# Create SimDataList object ----
############################################################

# The first argument to create_simulation_obj() is a
# SimDataList object. This is created with the constructor:
?sim_data_list

###########################
# data_list               #
###########################

# The first argument is a list of lists of matrices. Let's
# create this using our new function sim_single_matrix().
# For this example, we'll vary:
## 2 true HRs: 0.6 and 1.0
## 2 drift HRs: 1.0 and 1.5
# We'll do 100 simulations per scenario
n_datasets_per_sim <- 100 # (for a real study, you may consider many more)

# set seed so we all get the same results
set.seed(123)

# create list of lists of data with replicate
my_data_list <- list(
  replicate(
    n_datasets_per_sim,
    sim_single_matrix(n = 250, hr = 0.6, inherent_drift_hr = 1.0),
    simplify = FALSE
  ),
  replicate(
    n_datasets_per_sim,
    sim_single_matrix(n = 250, hr = 1.0, inherent_drift_hr = 1.0),
    simplify = FALSE
  ),
  replicate(
    n_datasets_per_sim,
    sim_single_matrix(n = 250, hr = 0.6, inherent_drift_hr = 1.5),
    simplify = FALSE
  ),
  replicate(
    n_datasets_per_sim,
    sim_single_matrix(n = 250, hr = 1.0, inherent_drift_hr = 1.5),
    simplify = FALSE
  )
)

# confirm we have a list of lists of matrices
NROW(my_data_list)
NROW(my_data_list[[1]])
head(my_data_list[[1]][[1]])

###########################
# guide                   #
###########################

# The second argument is a guide, which is simply a data.frame
# that indexes that parameters that differ at the highest level of
# the data_list. An example guide for us could be:

my_sim_data_guide <- expand.grid(
  true_hr = c(0.6, 1.0),
  drift_hr = c("No drift HR", "Moderate drift HR")
)

my_sim_data_guide$id <- seq(1, NROW(my_sim_data_guide))

my_sim_data_guide

###########################
# sim_data_list           #
###########################

# Now we have all the information we need to create a
# SimDataList object:

my_sim_data_list <- sim_data_list(
  data_list = my_data_list,
  guide = my_sim_data_guide,
  effect = "true_hr",
  drift = "drift_hr",
  index = "id"
)

my_sim_data_list

############################################################
# Create borrowing list object ----
############################################################

# For this example, let's assume we're mostly interested
# in comparing four different borrowing scenarios:
## No borrowing
## BDB with a conservative hyperprior
## BDB with an aggressive hyperprior
## Full borrowing
# We can specify these different scenarios with:
?sim_borrowing_list

my_borrowing_list <- sim_borrowing_list(
  list(
    "No borrowing" = borrowing_details("No borrowing", "ext"),
    "Full borrowing" = borrowing_details("Full borrowing", "ext"),
    "BDB - conservative" = borrowing_details("BDB", "ext", gamma_prior(0.001, 0.001)),
    "BDB - aggressive" = borrowing_details("BDB", "ext", gamma_prior(1, 0.001))
  )
)

############################################################
# Simulation object----
############################################################

# Suppose that we do not want to vary any other parameters.
# We can now create our simulation object. For any parameter
# that is not varied in the simulation study, the same inputs
# as create_analysis_obj() still apply!

simulation_obj <- create_simulation_obj(
  my_sim_data_list,
  outcome = exp_surv_dist("time", "cnsr", baseline_prior = normal_prior(0, 10000)),
  borrowing = my_borrowing_list,
  treatment = treatment_details(trt_flag_col = "trt", trt_prior = normal_prior(0, 10000))
)
simulation_obj

############################################################
# MCMC sampling----
############################################################

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
  scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0, 1)) +
  geom_hline(aes(yintercept = 0.05), linetype = 2)

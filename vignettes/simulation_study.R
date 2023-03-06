## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#",
  fig.width = 5,
  fig.height = 3,
  dpi = 120,
  fig.align = "center"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup, message = FALSE---------------------------------------------------
library(psborrow2)

## ----class.source = "fold-hide"-----------------------------------------------
library(simsurv)
# function to create a single matrix
sim_single_matrix <- function(n = 500, # n simulated pts
                              prob = c(
                                0.1, # proportion internal control
                                0.2, # proportion internal treated
                                0.7
                              ), # proportion external control
                              hr = 0.70, # true HR for the treatment
                              drift_hr = 1.0 # HR of external/internal
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
      ext = log(drift_hr)
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

## -----------------------------------------------------------------------------
head(sim_single_matrix(n = 500, hr = 0.5, drift_hr = 1.2))

## -----------------------------------------------------------------------------
# Seed for reproducibility
set.seed(123)

# Number of simulations per scenario
n <- 100

# Create list of lists of data
my_data_list <- list(
  replicate(n,
    sim_single_matrix(n = 250, hr = 0.6, drift_hr = 1.0),
    simplify = FALSE
  ),
  replicate(n,
    sim_single_matrix(n = 250, hr = 1.0, drift_hr = 1.0),
    simplify = FALSE
  ),
  replicate(n,
    sim_single_matrix(n = 250, hr = 0.6, drift_hr = 1.5),
    simplify = FALSE
  ),
  replicate(n,
    sim_single_matrix(n = 250, hr = 1.0, drift_hr = 1.5),
    simplify = FALSE
  )
)

## -----------------------------------------------------------------------------
NROW(my_data_list)

## -----------------------------------------------------------------------------
NROW(my_data_list[[1]])

## -----------------------------------------------------------------------------
head(my_data_list[[1]][[1]])

## -----------------------------------------------------------------------------
my_sim_data_guide <- expand.grid(
  true_hr = c(0.6, 1.0),
  drift_hr = c("No drift HR", "Moderate drift HR")
)

my_sim_data_guide$id <- seq(1, NROW(my_sim_data_guide))

my_sim_data_guide

## -----------------------------------------------------------------------------
my_sim_data_list <- sim_data_list(
  data_list = my_data_list,
  guide = my_sim_data_guide,
  effect = "true_hr",
  drift = "drift_hr",
  index = "id"
)

my_sim_data_list

## -----------------------------------------------------------------------------
my_sim_out <- exp_surv_dist(
  time_var = "time",
  cens_var = "cnsr",
  baseline_prior = normal_prior(0, 1000)
)

my_sim_out

## -----------------------------------------------------------------------------
my_borrowing_list <- sim_borrowing_list(
  list(
    "No borrowing" = borrowing_details("No borrowing", "ext"),
    "Full borrowing" = borrowing_details("Full borrowing", "ext"),
    "BDB - conservative" = borrowing_details("BDB", "ext", gamma_prior(0.001, 0.001)),
    "BDB - aggressive" = borrowing_details("BDB", "ext", gamma_prior(1, 0.001))
  )
)

my_borrowing_list

## -----------------------------------------------------------------------------
my_sim_treat <- treatment_details("trt", normal_prior(0, 1000))

my_sim_treat

## -----------------------------------------------------------------------------
simulation_obj <- create_simulation_obj(
  my_sim_data_list,
  outcome = my_sim_out,
  borrowing = my_borrowing_list,
  treatment = my_sim_treat
)

simulation_obj

## -----------------------------------------------------------------------------
show_guide(simulation_obj)

## ---- include = FALSE---------------------------------------------------------
st <- Sys.time()
simulation_res <- mcmc_sample(
  simulation_obj,
  posterior_quantiles = c(0.025, 0.975),
  iter_warmup = 400,
  iter_sampling = 1000,
  chains = 1L,
  seed = 112233
)
en <- Sys.time()

## ---- eval = FALSE------------------------------------------------------------
#  simulation_res <- mcmc_sample(
#    simulation_obj,
#    posterior_quantiles = c(0.025, 0.975),
#    iter_warmup = 400,
#    iter_sampling = 1000,
#    chains = 1L,
#    seed = 112233
#  )

## -----------------------------------------------------------------------------
class(simulation_res)

## -----------------------------------------------------------------------------
simulation_res_df <- get_results(simulation_res)
head(simulation_res_df)

## -----------------------------------------------------------------------------
# Load ggplot2
library(ggplot2)

# Factorize
simulation_res_df$borrowing_scenario <- factor(simulation_res_df$borrowing_scenario,
  levels = c(
    "No borrowing",
    "BDB - conservative",
    "BDB - aggressive",
    "Full borrowing"
  )
)

## ----class.source = "fold-hide", fig.dim = c(5, 3), dpi = 140-----------------
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

## ----class.source = "fold-hide", fig.dim = c(5, 3), dpi = 140-----------------
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

## ----class.source = "fold-hide", fig.dim = c(5, 3), dpi = 140-----------------
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


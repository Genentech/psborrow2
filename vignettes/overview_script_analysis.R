############################################################
#                                                          #
#         OVERVIEW OF BDB ANALYSIS IN PSBORROW2            #
#                  Matthew Secrest                         #
#                   October 2022                           #
#                                                          #
############################################################

# The goal of this demo is to conduct a Bayesian Dynamic Borrowing
# (BDB) analysis on a dataset

# Load packages ----
library(psborrow2)

# Survival analysis
library(survival)
library(survminer)
library(flexsurv)

# Additional tools for draws objects
library(bayesplot)
library(posterior)

# Comparing populations
library(table1)

############################################################
# Explore example data ----
############################################################

# psborrow2 contains an example matrix
head(example_matrix)
?example_matrix # true HR = 0.70 for trt = 1 vs trt = 0

# Load as data.frame for some functions
example_dataframe <- as.data.frame(example_matrix)

# Look at distribution of arms
table(ext = example_matrix[, "ext"], trt = example_matrix[, "trt"])

############################################################
# Naive internal comparisons ----
############################################################

# Kaplan-Meier curves
km_fit <- survfit(Surv(time = time, event = 1 - cnsr) ~ trt + ext,
  data = example_dataframe
)

ggsurvplot(km_fit) # The internal and external control arms look quite different

## Cox model
cox_fit <- coxph(Surv(time = time, event = 1 - cnsr) ~ trt,
  data = example_dataframe,
  subset = ext == 0
)

cox_fit
exp(confint(cox_fit)) # The internal HR is 0.90 (95% CI 0.61 - 1.32)

############################################################
# Hybrid control analysis----
############################################################

# Let's start by demonstrating the utility of BDB by trying to
# borrow data from the external control arm which we know
# experiences worse survival.

# The end goal is to create an Analysis object with:
?create_analysis_obj

############################################################
# A note on prior distributions ----
############################################################
# psborrow2 allows the user to specify priors with the following
# functions:
?bernoulli_prior
?beta_prior
?cauchy_prior
?exponential_prior
?gamma_prior
?normal_prior
?poisson_prior
?uniform_prior

# Prior distributions can be plotted with the plot() method
plot(normal_prior(0, 1), xlim = c(-100, 100), ylim = c(0, 1))
plot(normal_prior(0, 10), xlim = c(-100, 100), ylim = c(0, 1))
plot(normal_prior(0, 10000), xlim = c(-100, 100), ylim = c(0, 1))

############################################################
# Outcome objects----
############################################################

# psborrow2 currently supports 3 outcomes:
?outcome_surv_exponential
?outcome_surv_weibull_ph
?outcome_bin_logistic

# Create an exponential survival distribution Outcome object
exp_outcome <- outcome_surv_exponential(
  time_var = "time",
  cens_var = "cnsr",
  baseline_prior = normal_prior(0, 10000)
)

############################################################
# Borrowing objects ----
############################################################

# Borrowing objects are created with:
?borrowing_details

bdb_borrowing <- borrowing_details(
  method = "BDB",
  ext_flag_col = "ext",
  tau_prior = gamma_prior(0.001, 0.001)
)

############################################################
# Treatment objects ----
############################################################

# Treatment objects are created with:
?treatment_details

trt_details <- treatment_details(
  trt_flag_col = "trt",
  trt_prior = normal_prior(0, 10000)
)

############################################################
# Analysis objects ----
############################################################

# Combine everything and create object of class Analysis:
analysis_object <- create_analysis_obj(
  data_matrix = example_matrix,
  outcome = exp_outcome,
  borrowing = bdb_borrowing,
  treatment = trt_details
)

analysis_object

############################################################
# MCMC sampling----
############################################################

# Conduct MCMC sampling with:
?mcmc_sample

results <- mcmc_sample(
  x = analysis_object,
  iter_warmup = 1000,
  iter_sampling = 1000,
  chains = 1
)

class(results)

results

############################################################
# Interpret results ----
############################################################

# Dictionary to interpret parameters
variable_dictionary(analysis_object)

# Create draws object
draws <- results$draws()

# Rename draws object parameters
draws <- rename_draws_covariates(draws, analysis_object)

# Get 95% credible intervals with posterior package
posterior::summarize_draws(draws, ~ quantile(.x, probs = c(0.025, 0.50, 0.975)))

# Look at histogram of draws with bayesplot package
bayesplot::mcmc_hist(draws, c("treatment HR"))

# Our model does not borrow much from the external arm!
# This is the desired outcome given how different the control arms were.

############################################################
# Control arm imbalances ----
############################################################

# Check balance between arms
table1(
  ~ cov1 + cov2 + cov3 + cov4 |
    factor(ext, labels = c("Internal RCT", "External data")) +
      factor(trt, labels = c("Not treated", "Treated")),
  data = example_dataframe
)

## Because the imbalance may be conditional on observed covariates,
## let's adjust for propensity scores in our analysis

# Create a propensity score model
ps_model <- glm(ext ~ cov1 + cov2 + cov3 + cov4,
  data = example_dataframe,
  family = binomial
)
ps <- predict(ps_model, type = "response")
example_dataframe$ps <- ps
example_dataframe$ps_cat_ <- cut(
  example_dataframe$ps,
  breaks = 5,
  include.lowest = TRUE
)
levels(example_dataframe$ps_cat_) <- c(
  "ref", "low",
  "low_med", "high_med", "high"
)

## Convert the data back to a matrix with dummy variables for `ps_cat_` levels
example_matrix_ps <- create_data_matrix(
  example_dataframe,
  outcome = c("time", "cnsr"),
  trt_flag_col = "trt",
  ext_flag_col = "ext",
  covariates = ~ps_cat_
)

############################################################
# Propensity score analysis without borrowing ----
############################################################
anls_ps_no_borrow <- create_analysis_obj(
  data_matrix = example_matrix_ps,
  covariates = add_covariates(
    c("ps_cat_low", "ps_cat_low_med", "ps_cat_high_med", "ps_cat_high"),
    normal_prior(0, 10000)
  ),
  outcome = outcome_surv_exponential("time", "cnsr", normal_prior(0, 10000)),
  borrowing = borrowing_details("No borrowing", "ext"),
  treatment = treatment_details("trt", normal_prior(0, 10000))
)

res_ps_no_borrow <- mcmc_sample(
  x = anls_ps_no_borrow,
  iter_warmup = 1000,
  iter_sampling = 500,
  chains = 1
)

draws_ps_no_borrow <- rename_draws_covariates(
  res_ps_no_borrow$draws(),
  anls_ps_no_borrow
)

summarize_draws(draws_ps_no_borrow, ~ quantile(.x, probs = c(0.025, 0.50, 0.975)))

############################################################
# Propensity score analysis with BDB ----
############################################################

anls_ps_bdb <- create_analysis_obj(
  data_matrix = example_matrix_ps,
  covariates = add_covariates(
    c("ps_cat_low", "ps_cat_low_med", "ps_cat_high_med", "ps_cat_high"),
    normal_prior(0, 10000)
  ),
  outcome = outcome_surv_exponential("time", "cnsr", normal_prior(0, 10000)),
  borrowing = borrowing_details("BDB", "ext", gamma_prior(0.001, 0.001)),
  treatment = treatment_details("trt", normal_prior(0, 10000))
)

res_ps_bdb <- mcmc_sample(
  x = anls_ps_bdb,
  iter_warmup = 1000,
  iter_sampling = 500,
  chains = 1
)

draws_ps_bdb <- rename_draws_covariates(
  res_ps_bdb$draws(),
  anls_ps_bdb
)

summarize_draws(draws_ps_bdb, ~ quantile(.x, probs = c(0.025, 0.50, 0.975)))

## It looks like PS + BDB allowed us to most accurately recover the
## true hazard ratio of 0.70.

############################################################
#                                                          #
#         OVERVIEW OF BDB ANALYSIS IN PSBORROW2            #
#                  Matthew Secrest                         #
#                   October 2022                           #
#                                                          #
############################################################

# Goal of this demo is to:
  # Do a Bayesian Dynamic Borrowing analysis on a custom dataset

# Load dependancies----
# psborrow2
library(psborrow2)

# survival analysis
library(survival)
library(survminer)
library(flexsurv)

# additional tools for draws objects
library(bayesplot)
library(posterior)

# comparing populations
library(table1)

# plotting results
library(bayesplot)

############################################################
# Explore example data ----
############################################################

## psborrow2 contains an example matrix
?example_matrix
head(example_matrix)

## load as data.frame for some functions
example_dataframe <- as.data.frame(example_matrix)

# Distribution of arms
table(ext = example_matrix[, "ext"], trt = example_matrix[, "trt"])

# -------------------------------------------------------------------------


############################################################
# Naive internal comparisons ----
############################################################

## Cox model
cox_fit <- coxph(Surv(time = time, event = 1 - cnsr) ~ trt,
                 data = example_dataframe,
                 subset = ext == 0)

exp(confint(cox_fit))

## Kaplan-meier curves
km_fit <- survfit(Surv(time = time, event = 1 - cnsr) ~ trt,
                  data = example_dataframe,
                  subset = ext == 0)

ggsurvplot(km_fit)

## Exponential survival distribution
exp_fit <- flexsurvreg(Surv(time = time, event = 1 - cnsr) ~ trt,
                       dist = "exponential",
                       data = example_dataframe,
                       subset = ext == 0)

exp(confint(exp_fit))

############################################################
# Hybrid control analysis----
############################################################

## The end goal
?create_analysis_obj

## Outcome class----
?exp_surv_dist
?weib_ph_surv_dist
?logistic_bin_outcome

### A side note on priors
?bernoulli_prior
?beta_prior
?cauchy_prior
?exponential_prior
?gamma_prior
?normal_prior
?poisson_prior
?uniform_prior

### Plotting priors
plot(normal_prior(0, 1), xlim = c(-100, 100), ylim = c(0, 1))
plot(normal_prior(0, 10), xlim = c(-100, 100), ylim = c(0, 1))
plot(normal_prior(0, 10000), xlim = c(-100, 100), ylim = c(0, 1))

### Create Outcome object
exp_outcome <- exp_surv_dist(time_var = "time",
                             cens_var = "cnsr",
                             baseline_prior = normal_prior(0, 10000))

## Borrowing class ----
?borrowing_details

bdb_borrowing <- borrowing_details(method = "BDB",
                                   ext_flag_col = "ext",
                                   tau_prior = gamma_prior(0.001, 0.001))

## Treatment class ----
?treatment_details

trt_details <- treatment_details(trt_flag_col = "trt",
                                 trt_prior = normal_prior(0, 10000))

## Analysis class object ----
analysis_object <- create_analysis_obj(
  data_matrix = example_matrix,
  outcome = exp_outcome,
  borrowing = bdb_borrowing,
  treatment = trt_details
)

analysis_object

## Sample from the MCMC sampler----
?mcmc_sample

results <- mcmc_sample(
  x = analysis_object,
  iter_warmup = 1000,
  iter_sampling = 10000,
  chains = 2
)

class(results)

results$summary()

## Dictionary to interpret results ----
variable_dictionary(analysis_object)

## Evaluate draws object----
### Create draws object
draws <- results$draws()

### Get 95% posterior credible intervals
summarize_draws(draws, ~ quantile(.x, probs = c(0.025, 0.975)))

### Look at histogram of draws
mcmc_hist(draws, c("HR_trt"))

# Why did our model not borrow much from the external arm?
ggsurvplot(
  survfit(Surv(time, 1-cnsr) ~ ext,
          example_dataframe,
          subset = trt == 0)
)

############################################################
# Maybe our control arms are fundamentally different ----
############################################################

## Balance between cohorts
table1(~ cov1 + cov2 | trt + ext, data = example_dataframe)

ggsurvplot(
  survfit(Surv(time, 1-cnsr) ~ cov1,
          example_dataframe,
          subset = trt == 0)
)

## Let's incorporate propensity scores into our analysis
ps_model <- glm(ext ~ cov1 + cov2, data = example_dataframe, family = binomial)
ps <- predict(ps_model, type = "response")
example_dataframe$ps <- ps
head(example_dataframe)

example_dataframe$ps_cat <- cut(example_dataframe$ps,
                                breaks = c(0,.2,.4,.8,1),
                                include.lowest = T)

example_matrix_ps <- create_data_matrix(
  example_dataframe,
  outcome = c("time", "cnsr"),
  trt_flag_col = "trt",
  ext_flag_col = "ext",
  covariates = ~ ps_cat
)

### No borrowing
anls_ps_no_borrow <- create_analysis_obj(
  data_matrix = example_matrix_ps,
  outcome = exp_surv_dist("time", "cnsr", normal_prior(0, 10000)),
  borrowing = borrowing_details("No borrowing", "ext"),
  treatment = treatment_details("trt", normal_prior(0, 10000)),
  covariates = add_covariates(c("ps_cat(0.2,0.4]", "ps_cat(0.4,0.8]", "ps_cat(0.8,1]"), normal_prior(0, 10000))
)

res_ps_no_borrow <- mcmc_sample(
  x = anls_ps_no_borrow,
  iter_warmup = 1000,
  iter_sampling = 10000,
  chains = 2
)
summarize_draws(res_ps_no_borrow$draws(),  ~ quantile(.x, probs = c(0.025, 0.975)))

### BDB
anls_ps_bdb <- create_analysis_obj(
  data_matrix = example_matrix_ps,
  outcome = exp_surv_dist("time", "cnsr", normal_prior(0, 10000)),
  borrowing = borrowing_details("BDB", "ext", gamma_prior(0.001, 0.001)),
  treatment = treatment_details("trt", normal_prior(0, 10000)),
  covariates = add_covariates(c("ps_cat(0.2,0.4]", "ps_cat(0.4,0.8]", "ps_cat(0.8,1]"), normal_prior(0, 10000))
)

res_ps_bdb <- mcmc_sample(
  x = anls_ps_bdb,
  iter_warmup = 1000,
  iter_sampling = 10000,
  chains = 2
)

summarize_draws(anls_ps_bdb$draws(),  ~ quantile(.x, probs = c(0.025, 0.975)))

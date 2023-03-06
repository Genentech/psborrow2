## ----knitr, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.align = "center",
  fig.width = 5,
  fig.height = 3,
  dpi = 120,
  comment = "#"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----install-cmdstan-2, eval=FALSE--------------------------------------------
#  # Install the cmdstanr package
#  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
#  library(cmdstanr)
#  
#  # Install the external CmdStan program
#  check_cmdstan_toolchain()
#  install_cmdstan(cores = 2)

## ----dependencies, message = FALSE--------------------------------------------
library(psborrow2)

## ---- eval = require(survival)------------------------------------------------
# Start with data.frame
diabetic_df <- survival::diabetic

# For demonstration purposes, let some patients be external controls
diabetic_df$external <- ifelse(diabetic_df$trt == 0 & diabetic_df$id > 1000, 1, 0)

# Create the censor flag
diabetic_df$cens <- ifelse(diabetic_df$status == 0, 1, 0)

diabetes_matrix <- create_data_matrix(
  diabetic_df,
  outcome = c("time", "cens"),
  trt_flag_col = "trt",
  ext_flag_col = "external",
  covariates = ~ age + laser + risk
)

head(diabetes_matrix)

## ----example-matrix-----------------------------------------------------------
head(example_matrix)

## ----exp-surv-dist------------------------------------------------------------
outcome <- exp_surv_dist(
  time_var = "time",
  cens_var = "cnsr",
  baseline_prior = normal_prior(0, 1000)
)
outcome

## ----borrowing-details--------------------------------------------------------
borrowing <- borrowing_details(
  method = "BDB",
  ext_flag_col = "ext",
  tau_prior = gamma_prior(0.001, 0.001)
)
borrowing

## ----treatment-details--------------------------------------------------------
treatment <- treatment_details(
  trt_flag_col = "trt",
  trt_prior = normal_prior(0, 1000)
)
treatment

## ----anls-obj-----------------------------------------------------------------
anls_obj <- create_analysis_obj(
  data_matrix = example_matrix,
  outcome = outcome,
  borrowing = borrowing,
  treatment = treatment
)

## ----res----------------------------------------------------------------------
results <- mcmc_sample(anls_obj,
  iter_warmup = 2000,
  iter_sampling = 50000,
  chains = 4,
  seed = 112233
)

## ----res-summary--------------------------------------------------------------
results$summary()

## ----data-dict----------------------------------------------------------------
variable_dictionary(anls_obj)

## ----draws--------------------------------------------------------------------
draws <- results$draws()
print(draws)

## ----rename-draws-------------------------------------------------------------
draws <- rename_draws_covariates(draws, anls_obj)
summary(draws)

## ----mcmc-hist, message = FALSE, eval = requireNamespace("bayesplot")---------
bayesplot::mcmc_hist(draws, c("treatment HR", "commensurability parameter"))

## ----mcmc-trace, fig.width=6, fig.height=3.6, dpi = 100, eval = requireNamespace("bayesplot")----
bayesplot::color_scheme_set("mix-blue-pink")

bayesplot::mcmc_trace(
  draws[1:5000, 1:2, ], # Using a subset of draws only
  pars = c("treatment HR", "commensurability parameter"),
  n_warmup = 1000
)

## ----posterior-summ, message = FALSE------------------------------------------
library(posterior)
summarize_draws(draws, ~ quantile(.x, probs = c(0.1, 0.9)))

## ----mcse-mean----------------------------------------------------------------
vm <- extract_variable_matrix(draws, "treatment HR")
mcse_quantile(x = vm, probs = c(0.1, 0.5, 0.9))


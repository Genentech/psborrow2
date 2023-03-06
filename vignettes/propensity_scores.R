## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 4,
  fig.align = "center",
  messages = FALSE,
  warnings = FALSE
)

## ----load, message=FALSE------------------------------------------------------
library(psborrow2)

## ----data---------------------------------------------------------------------
example_dataframe <- as.data.frame(example_matrix)
example_dataframe$int <- 1 - example_dataframe$ext

## ----glm----------------------------------------------------------------------
ps_model <- glm(int ~ cov1 + cov2 + cov3 + cov4,
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

## ----include = FALSE----------------------------------------------------------
example_matrix_ps <- create_data_matrix(
  example_dataframe,
  outcome = c("time", "cnsr"),
  trt_flag_col = "trt",
  ext_flag_col = "ext",
  covariates = ~ ps_cat_ + ps
)

## ----analysis_obj-------------------------------------------------------------
analysis_ps_cuts <- create_analysis_obj(
  data_matrix = example_matrix_ps,
  covariates = add_covariates(
    c("ps_cat_low", "ps_cat_low_med", "ps_cat_high_med", "ps_cat_high"),
    priors = normal_prior(0, 10000)
  ),
  outcome = exp_surv_dist("time", "cnsr", normal_prior(0, 10000)),
  borrowing = borrowing_details("Full borrowing", "ext"),
  treatment = treatment_details("trt", normal_prior(0, 10000))
)

result_ps_cuts <- mcmc_sample(analysis_ps_cuts)


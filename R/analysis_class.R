# class union ----
setClassUnion("CovariatesOrNULL", c("Covariates", "NULL"))

#' `Analysis` Class
#'
#' A class for defining Analysis details. Objects of class
#' `Analysis` should not be created directly but by the constructor
#' `analysis_details()`.
#'
#' @slot model_matrix matrix. The data matrix, including all covariates to be
#' adjusted for, all relevant outcome variables, and treatment arm and external
#' control arm flags.
#' @slot covariates `Covariate`. Object of class `Covariate` as output by
#' the function `covariate_details()`.
#' @slot outcome `Outcome`. Object of class `Outcome` as output by
#' `exp_surv_dist()`, `weib_ph_surv_dist()`, or `logistic_bin_outcome()`.
#' @slot borrowing `Borrowing`. Object of class `Borrowing` as output by
#' `borrowing_details()`.
#' @slot treatment `Treatment`. Object of class `Treatment` as output by
#' `treatment_details()`.
#' @slot model_string character. The string that contains the full
#' Stan model code to be compiled.
#' @slot model_and_data. A named list containing two items: 1) `stan_model`,
#' the compiled Stan model as output by cmdstanr::cmdstan_model, and
#' 2) `data_in`, a named list of inputs that will be passed to the compiled model.
#' @include covariate_class.R
#' @include outcome_class.R
#' @include borrowing_class.R
#' @include treatment_class.R
#' @import cmdstanr
.analysis_obj <- setClass(
  "Analysis",
  slots = c(
    model_matrix = "matrix",
    covariates = "CovariatesOrNULL",
    outcome = "Outcome",
    borrowing = "Borrowing",
    treatment = "Treatment",
    model_string = "character",
    model_and_data = "list"
  )
)
# show ----
setMethod(
  f = "show",
  signature = "Analysis",
  definition = function(object) {
    cat(
      "Analysis object (compiled and ready to sample)",
      "Call mcmc_sample() next."
    )
  }
)

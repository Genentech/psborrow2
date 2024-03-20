# class union ----
setClassUnion("CovariatesOrNULL", c("Covariates", "NULL"))

# R6 CmdStanModel placeholder
setClass("CmdStanModel")
setClassUnion("CmdStanModelOrNULL", c("CmdStanModel", "NULL"))

#' `Analysis` Class
#'
#' A class for defining Analysis details. Objects of class
#' `Analysis` should not be created directly but by the constructor
#' `create_analysis_obj()`.
#'
#' @slot data_matrix matrix. The data matrix, including all covariates to be
#' adjusted for, all relevant outcome variables, and treatment arm and external
#' control arm flags.
#' @slot covariates `Covariate`. Object of class `Covariate` as output by
#' the function `covariate_details()`.
#' @slot outcome `Outcome`. Object of class `Outcome` as output by
#' `outcome_surv_exponential()`, `outcome_surv_weibull_ph()`, or `outcome_bin_logistic()`.
#' @slot borrowing `Borrowing`. Object of class `Borrowing` as output by
#' `borrowing_full()`, `borrowing_none()`, or `borrowing_hierarchical_commensurate()`.
#' @slot treatment `Treatment`. Object of class `Treatment` as output by
#' `treatment_details()`.
#' @slot model_string character. The string that contains the full
#' Stan model code to be compiled.
#' @slot model `CmdStanModel`. The compiled Stan model as output by `cmdstanr::cmdstan_model()`
#' @slot ready_to_sample logical. Is the object ready to sample?
#' @include covariate_class.R
#' @include outcome_class.R
#' @include borrowing_class.R
#' @include treatment_class.R
.analysis_obj <- setClass(
  "Analysis",
  slots = c(
    data_matrix = "matrix",
    covariates = "CovariatesOrNULL",
    outcome = "Outcome",
    borrowing = "Borrowing",
    treatment = "Treatment",
    model_string = "character",
    model = "CmdStanModelOrNULL",
    ready_to_sample = "logical"
  ),
  prototype = list(
    ready_to_sample = FALSE
  )
)
# show ----
setMethod(
  f = "show",
  signature = "Analysis",
  definition = function(object) {
    cat("Analysis Object\n\n")

    cat("Outcome model:", class(object@outcome)[1], "\n")
    outcome_vars <- get_vars(object@outcome)
    cat("Outcome", ifelse(length(outcome_vars) > 1, "variables:", "variable:"), outcome_vars, "\n\n")

    cat("Borrowing method:", object@borrowing@method_name, "\n")
    cat("External flag:", get_vars(object@borrowing), "\n\n")

    cat("Treatment variable:", get_vars(object@treatment), "\n\n")

    cov_names <- get_vars(object@covariates)
    if (!is.null(cov_names)) cat("Covariates:", cov_names, "\n\n")

    cat("Data: Matrix with", nrow(object@data_matrix), "observations \n")
    cat(
      "    - ", sum(object@data_matrix[, get_vars(object@treatment)] == 0 &
        object@data_matrix[, get_vars(object@borrowing)["ext_flag_col"]] == 0),
      " internal controls\n"
    )
    cat(
      "    - ", sum(object@data_matrix[, get_vars(object@treatment)] == 0 &
        object@data_matrix[, get_vars(object@borrowing)["ext_flag_col"]] == 1),
      " external controls", ifelse(is(object@borrowing, "BorrowingNone"),
        " (ignored in this analysis)\n",
        "\n"
      )
    )
    cat(
      "    - ", sum(object@data_matrix[, get_vars(object@treatment)] == 1 &
        object@data_matrix[, get_vars(object@borrowing)["ext_flag_col"]] == 0),
      " internal experimental\n\n"
    )

    if (object@ready_to_sample == TRUE) {
      cat(
        "Stan model compiled and ready to sample.\n",
        "Call mcmc_sample() next."
      )
    } else {
      cat("Not ready to sample yet.")
    }
  }
)

# get_vars ----
#' @rdname get_vars
#' @include generics.R
setMethod(
  f = "get_vars",
  signature = "Analysis",
  definition = function(object) {
    c(
      get_vars(object@outcome),
      get_vars(object@borrowing),
      get_vars(object@treatment),
      get_vars(object@covariates)
    )
  }
)

#' @rdname get_vars
#' @include generics.R
#' @usage \S4method{get_vars}{NULL}(object)
setMethod(
  f = "get_vars",
  signature = "NULL",
  definition = function(object) {
    NULL
  }
)

# get_stan_code ----
#' @rdname get_stan_code
#' @include generics.R
setMethod(
  f = "get_stan_code",
  signature = "Analysis",
  definition = function(object) {
    if (is.null(object@model_string)) {
      stop("Model not compiled yet!")
    } else {
      object@model_string
    }
  }
)

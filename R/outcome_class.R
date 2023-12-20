#' `Outcome` class
#' @family outcome
setClass(
  "Outcome",
  contains = "VIRTUAL"
)

setClassUnion(
  "vectorOrNULL",
  c("vector", "NULL")
)

#' `TimeToEvent` class
#'
#' @slot function_stan_code character. Code to include in the Stan functions program block.
#' @slot param_stan_code character. Code to include in the Stan parameters program block.
#' @slot likelihood_stan_code character. Code defining the likelihood to include in the Stan model program block.
#' @slot data_stan_code character. Code to include in the Stan data program block.
#' @slot n_param integer. Number of ancillary parameters for the model to estimate.
#' @slot param_priors list. Named list of prior distributions on the ancillary parameters in the model.
#' @slot time_var character. Variable used for time in `TimeToEvent` objects.
#' @slot cens_var character. Variable used for censoring in `TimeToEvent` objects.
#' @slot baseline_prior `Prior`. Object of class `Prior`
#' specifying prior distribution for the baseline outcome.
#' @slot name_beta_trt. Named vector for beta_trt.
#' @slot name_exp_trt. Named vector for exponentiated beta_trt
#' @slot alpha_type. How to interpret alpha.
#' @slot name_addnl_params. Named vector for additional parameters.
#' @family outcome
setClass(
  "TimeToEvent",
  slots = c(
    function_stan_code = "character",
    param_stan_code = "character",
    likelihood_stan_code = "character",
    data_stan_code = "character",
    n_param = "integer",
    param_priors = "list",
    time_var = "character",
    cens_var = "character",
    weight_var = "character",
    baseline_prior = "Prior",
    name_beta_trt = "vector",
    name_exp_trt = "vector",
    alpha_type = "character",
    name_addnl_params = "vectorOrNULL"
  ),
  prototype = list(
    n_param = 0L,
    function_stan_code = "",
    param_stan_code = "",
    likelihood_stan_code = "",
    weight_var = NULL,
    data_stan_code = "vector[N] time;
    vector[N] cens;",
    baseline_prior = NULL,
    name_beta_trt = c("treatment log HR" = "beta_trt"),
    name_exp_trt = c("treatment HR" = "HR_trt"),
    alpha_type = "baseline log hazard rate",
    name_addnl_params = NULL
  ),
  contains = "Outcome"
)

#' `BinaryOutcome` class
#' @slot function_stan_code character. Code to include in the Stan functions program block.
#' @slot param_stan_code character. Code to include in the Stan parameters program block.
#' @slot likelihood_stan_code character. Code defining the likelihood to include in the Stan model program block.
#' @slot data_stan_code character. Code to include in the Stan data program block.
#' @slot n_param integer. Number of ancillary parameters for the model to estimate.
#' @slot param_priors list. Named list of prior distributions on the ancillary parameters in the model.
#' @slot binary_var character. Variable used for outcome in `BinaryOutcome` objects.
#' @slot baseline_prior `Prior`. Object of class `Prior`
#' specifying prior distribution for the baseline outcome.
#' @slot name_beta_trt. Named vector for beta_trt.
#' @slot name_exp_trt. Named vector for exponentiated beta_trt
#' @slot alpha_type. How to interpret alpha.
#' @slot name_addnl_params. Named vector for additional parameters.
#' @family outcome
setClass(
  "BinaryOutcome",
  slots = c(
    function_stan_code = "character",
    param_stan_code = "character",
    likelihood_stan_code = "character",
    data_stan_code = "character",
    n_param = "integer",
    param_priors = "list",
    binary_var = "character",
    weight_var = "character",
    baseline_prior = "Prior",
    name_beta_trt = "vector",
    name_exp_trt = "vector",
    alpha_type = "character",
    name_addnl_params = "vectorOrNULL"
  ),
  prototype = list(
    n_param = 0L,
    function_stan_code = "",
    param_stan_code = "",
    likelihood_stan_code = "",
    weight_var = "",
    data_stan_code = "array[N] int y;",
    baseline_prior = NULL,
    name_beta_trt = c("treatment log OR" = "beta_trt"),
    name_exp_trt = c("treatment OR" = "OR_trt"),
    alpha_type = "intercept",
    name_addnl_params = NULL
  ),
  contains = "Outcome"
)

#' `ContinuousOutcome` class
#' @slot function_stan_code character. Code to include in the Stan functions program block.
#' @slot param_stan_code character. Code to include in the Stan parameters program block.
#' @slot likelihood_stan_code character. Code defining the likelihood to include in the Stan model program block.
#' @slot data_stan_code character. Code to include in the Stan data program block.
#' @slot n_param integer. Number of ancillary parameters for the model to estimate.
#' @slot param_priors list. Named list of prior distributions on the ancillary parameters in the model.
#' @slot continuous_var character. Variable used for outcome in `ContinuousOutcome` objects.
#' @slot baseline_prior `Prior`. Object of class `Prior`
#' specifying prior distribution for the baseline outcome.
#' @slot name_beta_trt. Named vector for beta_trt.
#' @slot name_exp_trt. Named vector for exponentiated beta_trt
#' @slot alpha_type. How to interpret alpha.
#' @slot name_addnl_params. Named vector for additional parameters.
#' @family outcome
setClass(
  "ContinuousOutcome",
  slots = c(
    function_stan_code = "character",
    param_stan_code = "character",
    likelihood_stan_code = "character",
    data_stan_code = "character",
    n_param = "integer",
    param_priors = "list",
    continuous_var = "character",
    weight_var = "character",
    baseline_prior = "Prior",
    name_beta_trt = "vector",
    name_exp_trt = "vector",
    alpha_type = "character",
    name_addnl_params = "vectorOrNULL"
  ),
  prototype = list(
    n_param = 0L,
    function_stan_code = "",
    param_stan_code = "",
    likelihood_stan_code = "",
    weight_var = "",
    data_stan_code = "array[N] int y;",
    baseline_prior = NULL,
    name_beta_trt = c("treatment effect" = "beta_trt"),
    name_exp_trt = c("exponentiated treatment effect" = "beta_trt"),
    alpha_type = "intercept",
    name_addnl_params = NULL
  ),
  contains = "Outcome"
)

# show ----
setMethod(
  f = "show",
  signature = "Outcome",
  definition = function(object) {
    cat("Outcome object with class", class(object)[1], "\n\n")
    cat("Outcome variables:\n")
    print(get_vars(object))
    cat("\n")
    cat("Baseline prior:\n")
    show(object@baseline_prior)

    if (!is.null(object@param_priors)) {
      cat("\n")
      for (i in names(object@param_priors)) {
        cat(i, "prior:\n")
        show(object@param_priors[[i]])
      }
    }
  }
)

# get_vars ----
#' @rdname get_vars
#' @include generics.R
setMethod(
  f = "get_vars",
  signature = "TimeToEvent",
  definition = function(object) {
    weight_var <- if (object@weight_var != "") object@weight_var
    c(time_var = object@time_var, cens_var = object@cens_var, weight_var = weight_var)
  }
)

#' @rdname get_vars
#' @include generics.R
setMethod(
  f = "get_vars",
  signature = "BinaryOutcome",
  definition = function(object) {
    weight_var <- if (object@weight_var != "") object@weight_var
    c(binary_var = object@binary_var, weight_var = weight_var)
  }
)

#' @rdname get_vars
#' @include generics.R
setMethod(
  f = "get_vars",
  signature = "ContinuousOutcome",
  definition = function(object) {
    weight_var <- if (object@weight_var != "") object@weight_var
    c(continuous_var = object@continuous_var, weight_var = weight_var)
  }
)

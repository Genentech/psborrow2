#' @include outcome_surv_piecewise_exponential.R
NULL

#' @rdname make_model_string_model
setMethod(
  "make_model_string_model",
  signature("ANY", "ANY", "Analysis"),
  function(borrowing, outcome, analysis_obj) {
    stop(h_glue("No Stan model defined for the combination of {{class(outcome)}} and {{class(borrowing)}}."))
  }
)

make_model_string_full_none <- function(borrowing, outcome, analysis_obj) {
  ### Treatment prior
  beta_trt_prior <- get_prior_string(analysis_obj@treatment@trt_prior)

  ### Linear predictor
  has_covariates <- !is.null(analysis_obj@covariates)

  if (has_covariates) {
    linear_predictor <- h_glue("
      lp = alpha + X * beta + trt * beta_trt ;
      elp = exp(lp) ;")
  } else if (!has_covariates) {
    linear_predictor <- h_glue("
      lp = alpha + trt * beta_trt ;
      elp = exp(lp);")
  }

  ### Add priors for relevant parameters
  if (NROW(analysis_obj@outcome@param_priors) > 0) {
    names <- names(analysis_obj@outcome@param_priors)
    values <- get_prior_string(analysis_obj@outcome@param_priors)
    outcome_prior <- h_glue("{{names}} ~ {{values}} ;", collapse = TRUE)
  } else {
    outcome_prior <- ""
  }

  ### Add priors on betas
  if (has_covariates) {
    i <- seq_along(analysis_obj@covariates@covariates)
    value <- get_prior_string(analysis_obj@covariates@priors)
    index <- if (test_named(value)) get_vars(analysis_obj@covariates) else rep(1, length(i))
    covariate_prior <- h_glue("beta[{{i}}] ~ {{value[index]}} ;", collapse = TRUE)
  } else {
    covariate_prior <- ""
  }

  alpha_prior <- get_prior_string(analysis_obj@outcome@baseline_prior)
  borrowing_string <- h_glue("alpha ~ {{alpha_prior}} ;")

  ### Add in likelihood function
  likelihood_string <- h_glue("{{analysis_obj@outcome@likelihood_stan_code}}")

  h_glue("
  model {
    vector[N] lp;
    vector[N] elp;
    beta_trt ~ {{beta_trt_prior}};
    {{linear_predictor}}
    {{outcome_prior}}
    {{covariate_prior}}
    {{borrowing_string}}
    {{likelihood_string}}
  }")
}

# BorrowingFull, ANY -------
#' @rdname make_model_string_model
setMethod("make_model_string_model", signature("BorrowingFull", "ANY", "Analysis"), make_model_string_full_none)

# BorrowingNone, ANY -------

#' @rdname make_model_string_model
setMethod("make_model_string_model", signature("BorrowingNone", "ANY", "Analysis"), make_model_string_full_none)

# BorrowingHierarchicalCommensurate, ANY -------

#' @rdname make_model_string_model
setMethod(
  "make_model_string_model",
  signature("BorrowingHierarchicalCommensurate", "ANY", "Analysis"),
  definition = function(borrowing, outcome, analysis_obj) {
    ### Treatment prior
    beta_trt_prior <- get_prior_string(analysis_obj@treatment@trt_prior)

    ### Linear predictor
    has_covariates <- !is.null(analysis_obj@covariates)

    if (has_covariates) {
      linear_predictor <- h_glue("
      lp = X * beta + Z * alpha + trt * beta_trt;
      elp = exp(lp) ;")
    } else if (!has_covariates) {
      linear_predictor <- h_glue("
      lp = Z * alpha + trt * beta_trt;
      elp = exp(lp) ;")
    }

    ### Add priors for relevant parameters
    if (NROW(analysis_obj@outcome@param_priors) > 0) {
      names <- names(analysis_obj@outcome@param_priors)
      values <- get_prior_string(analysis_obj@outcome@param_priors)
      outcome_prior <- h_glue("{{names}} ~ {{values}} ;", collapse = TRUE)
    } else {
      outcome_prior <- ""
    }

    ### Add priors on betas
    if (has_covariates) {
      i <- seq_along(analysis_obj@covariates@covariates)
      value <- get_prior_string(analysis_obj@covariates@priors)
      index <- if (test_named(value)) get_vars(analysis_obj@covariates) else rep(1, length(i))
      covariate_prior <- h_glue("beta[{{i}}] ~ {{value[index]}} ;", collapse = TRUE)
    } else {
      covariate_prior <- ""
    }

    tau_prior <- get_prior_string(analysis_obj@borrowing@tau_prior)
    alpha_2_prior <- get_prior_string(analysis_obj@outcome@baseline_prior)

    borrowing_string <- h_glue("
      tau ~ {{tau_prior}} ;
      real sigma;
      sigma = 1 / tau;
      alpha[2] ~ {{alpha_2_prior}} ;
      alpha[1] ~ normal(alpha[2], sqrt(sigma)) ;")

    ### Add in likelihood function
    likelihood_string <- h_glue("{{analysis_obj@outcome@likelihood_stan_code}}")

    h_glue("
  model {
    vector[N] lp;
    vector[N] elp;
    beta_trt ~ {{beta_trt_prior}};
    {{linear_predictor}}
    {{outcome_prior}}
    {{covariate_prior}}
    {{borrowing_string}}
    {{likelihood_string}}
  }")
  }
)

# BorrowingHierarchicalCommensurate, OutcomeSurvPiecewiseExponential -----
#' @rdname make_model_string_model
setMethod(
  "make_model_string_model",
  signature("BorrowingHierarchicalCommensurate", "OutcomeSurvPiecewiseExponential", "Analysis"),
  function(borrowing, outcome, analysis_obj) {
    ### Treatment prior
    beta_trt_prior <- get_prior_string(analysis_obj@treatment@trt_prior)

    ### Linear predictor
    has_covariates <- !is.null(analysis_obj@covariates)

    linear_predictor <- if (has_covariates) {
      h_glue("lp = Z*alpha' + rep_matrix(trt * beta_trt + X * beta, M);")
    } else if (!has_covariates) {
      h_glue("lp = Z*alpha' + rep_matrix(trt * beta_trt, M);")
    }

    ### Add priors for relevant parameters
    if (NROW(analysis_obj@outcome@param_priors) > 0) {
      names <- names(analysis_obj@outcome@param_priors)
      values <- get_prior_string(analysis_obj@outcome@param_priors)
      outcome_prior <- h_glue("{{names}} ~ {{values}} ;", collapse = TRUE)
    } else {
      outcome_prior <- ""
    }

    ### Add priors on betas
    if (has_covariates) {
      i <- seq_along(analysis_obj@covariates@covariates)
      value <- get_prior_string(analysis_obj@covariates@priors)
      index <- if (test_named(value)) get_vars(analysis_obj@covariates) else rep(1, length(i))
      covariate_prior <- h_glue("beta[{{i}}] ~ {{value[index]}} ;", collapse = TRUE)
    } else {
      covariate_prior <- ""
    }

    tau_prior <- get_prior_string(analysis_obj@borrowing@tau_prior)
    alpha_2_prior <- get_prior_string(analysis_obj@outcome@baseline_prior)

    borrowing_string <- h_glue("
      tau ~ {{tau_prior}} ;
      real sigma;
      sigma = 1 / tau;
      alpha[, 2] ~ {{alpha_2_prior}};
      alpha[, 1] ~ normal(alpha[,2], sqrt(sigma)) ;
      ")

    ### Add in likelihood function
    likelihood_string <- "target += sum((lp .* D) - (exp(lp) .* T));"

    h_glue("
  model {
    matrix[N,M] lp;
    vector[N] elp;
    beta_trt ~ {{beta_trt_prior}};
    {{linear_predictor}}
    {{outcome_prior}}
    {{covariate_prior}}
    {{borrowing_string}}
    {{likelihood_string}}
  }")
  }
)

# BorrowingNone, OutcomeSurvPiecewiseExponential -----
make_model_string_pem_full_none <- function(borrowing, outcome, analysis_obj) {
  ### Treatment prior
  beta_trt_prior <- get_prior_string(analysis_obj@treatment@trt_prior)

  ### Linear predictor
  has_covariates <- !is.null(analysis_obj@covariates)

  linear_predictor <- if (has_covariates) {
    h_glue("lp = rep_matrix(alpha, N) + rep_matrix(trt * beta_trt + X * beta, M);")
  } else if (!has_covariates) {
    h_glue("lp = rep_matrix(alpha, N) + rep_matrix(trt * beta_trt, M);")
  }

  ### Add priors for relevant parameters
  if (NROW(analysis_obj@outcome@param_priors) > 0) {
    names <- names(analysis_obj@outcome@param_priors)
    values <- get_prior_string(analysis_obj@outcome@param_priors)
    outcome_prior <- h_glue("{{names}} ~ {{values}} ;", collapse = TRUE)
  } else {
    outcome_prior <- ""
  }

  ### Add priors on betas
  if (has_covariates) {
    i <- seq_along(analysis_obj@covariates@covariates)
    value <- get_prior_string(analysis_obj@covariates@priors)
    index <- if (test_named(value)) get_vars(analysis_obj@covariates) else rep(1, length(i))
    covariate_prior <- h_glue("beta[{{i}}] ~ {{value[index]}} ;", collapse = TRUE)
  } else {
    covariate_prior <- ""
  }

  alpha_prior <- get_prior_string(analysis_obj@outcome@baseline_prior)
  borrowing_string <- h_glue("alpha ~ {{alpha_prior}};")

  ### Add in likelihood function
  likelihood_string <- "target += sum((lp .* D) - (exp(lp) .* T));"

  h_glue("
  model {
    matrix[N,M] lp;
    beta_trt ~ {{beta_trt_prior}};
    {{linear_predictor}}
    {{outcome_prior}}
    {{covariate_prior}}
    {{borrowing_string}}
    {{likelihood_string}}
  }")
}

#' @rdname make_model_string_model
setMethod(
  "make_model_string_model",
  signature("BorrowingNone", "OutcomeSurvPiecewiseExponential", "Analysis"),
  make_model_string_pem_full_none
)

#' @rdname make_model_string_model
setMethod(
  "make_model_string_model",
  signature("BorrowingFull", "OutcomeSurvPiecewiseExponential", "Analysis"),
  make_model_string_pem_full_none
)

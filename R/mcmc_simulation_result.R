# union ----
setClassUnion("listOrNULL", c("list", "NULL"))

#' `MCMCSimulationResult` Class
#'
#' A class for defining Simulation study results. Objects of class
#' `MCMCSimulationResult` should not be created directly but by
#' `mcmc_sample()`.
#'
#' @slot results `data.frame`. The results of the simulation study summarized
#' in a `data.frame`
#' @slot cmd_stan_models list. List of lists of `CmdStanmodels` corresponding to the
#' different parameters in `Simulation@guide` and different datasets in
#' `Simulation@data_matrix_list`.
#'
#' @include mcmc_sample.R
.mcmc_simulation_result <- setClass(
  "MCMCSimulationResult",
  slots = c(
    results = "data.frame",
    cmd_stan_models = "listOrNULL"
  ),
  prototype = list(
    cmd_stan_models = NULL
  )
)

# show ----
setMethod(
  f = "show",
  signature = "MCMCSimulationResult",
  definition = function(object) {
    cat(
      "`MCMCSimulationResult` object. ",
      "Call `get_results()` to save outputs as a data.frame"
    )
    object@results
  }
)

# get_results----
#' @rdname get_results
setMethod(
  f = "get_results",
  signature = "MCMCSimulationResult",
  definition = function(object) {
    res <- object@results
    res$trt_var <- vapply(res$trt_var, mean, double(1L))
    res$mse_mean <- vapply(res$mse_mean, mean, double(1L))
    res$bias_mean <- vapply(res$bias_mean, mean, double(1L))
    res$null_coverage <- vapply(res$null_coverage, mean, double(1L))
    res$true_coverage <- vapply(res$true_coverage, mean, double(1L))
    return(res)
  }
)

# get_cmd_stan_models----
#' @rdname get_cmd_stan_models
setMethod(
  f = "get_cmd_stan_models",
  signature = "MCMCSimulationResult",
  definition = function(object) {
    return(object@cmd_stan_models)
  }
)

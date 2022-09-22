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
#' @slot draws list. List of lists of `draws_arrays` corresponding to the
#' different parameters in `Simulation@guide` and different datasets in
#' `Simulation@data_matrix_list`.
#'
#' @include mcmc_sample.R
.mcmc_simulation_result <- setClass(
  "MCMCSimulationResult",
  slots = c(
    results = "data.frame",
    draws = "listOrNULL"
  ),
  prototype = list(
    draws = NULL
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
    head(object@results)
  }
)

# get_results----
#' @rdname get_results
setMethod(
  f = "get_results",
  signature = "MCMCSimulationResult",
  definition = function(object) {
    return(object@results)
  }
)

# get_draws----
#' @rdname get_draws
setMethod(
  f = "get_draws",
  signature = "MCMCSimulationResult",
  definition = function(object) {
    return(object@draws)
  }
)

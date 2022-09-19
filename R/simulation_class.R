# class union ----
setClassUnion("SimCovariateListOrNULL", c("SimCovariateList", "NULL"))

#' `Simulation` Class
#'
#' A class for defining Simulation study details. Objects of class
#' `Simulation` should not be created directly but by the constructor
#' `create_simulation_obj()`.
#'
#' @slot data_list `SimDataList`. The list of lists of data matrices created
#' with `sim_data_list()`.
#' @slot outcome_list `SimOutcomeList`. List of outcome objects created with
#' `sim_outcome_list()`.
#' @slot borrowing_list `SimBorrowingList`. List of borrowing objects created
#' with `sim_borrowing_list()`.
#' @slot covariate_list `SimCovariateList`. List of covariate objects created
#' with `sim_covariate_list()`.
#' @slot treatment_list `SimTreatmentList`. List of treatment objects created
#' with `sim_treatment_list()`.
#' @slot guide data.frame. Data.frame containing information on all
#' combinations evaluated.
#' @slot n_combos integer. Number of combinations of parameters to be evaluated.
#' @slot n_analyses integer. Number of analyses (combos x datasets to be performed).
#' @include sim_data_list.R
#' @include sim_covariate_list.R
#' @include sim_borrowing_list.R
#' @include sim_outcome_list.R
#' @include sim_treatment_list.R
.simulation_obj <- setClass(
  "Simulation",
  slots = c(
    data_list = "SimDataList",
    outcome_list = "SimOutcomeList",
    borrowing_list = "SimBorrowingList",
    covariate_list = "SimCovariateListOrNULL",
    treatment_list = "SimTreatmentList",
    guide = "data.frame",
    n_combos = "integer",
    n_analyses = "integer"
  ),
  prototype = list(
    n_combos = 0L,
    n_analyses = 0L
  )
)

# show ----
setMethod(
  f = "show",
  signature = "Simulation",
  definition = function(object) {
    if (object@n_combos > 20 | object@n_analyses > 100) {
      cat(
        "Simulation object with ",
        object@n_combos,
        " combinations and ",
        object@n_analyses,
        " analyses ready to sample. ",
        "This is a lot of combinations/analyses! Consider breaking the ",
        "study into different simulation objects. Or if you ",
        "prefer to use these combinations, call `mcmc_sample()` next."
      )
    } else {
      cat(
        "Simulation object with ",
        object@n_combos,
        " combinations and ",
        object@n_analyses,
        " analyses ready to sample. ",
        "Next, call `mcmc_sample()`!"
      )
    }
  }
)

# get_vars ----
#' @rdname get_vars
#' @include generics.R
setMethod(
  f = "get_vars",
  signature = "Simulation",
  definition = function(object) {
    cov_cols <- if (!is.null(object@covariate_list)) {
      unlist(lapply(object@covariate_list@covariate_list, get_vars))
    } else {
      NULL
    }
    names(cov_cols) <- NULL

    ext_cols <- unique(vapply(object@borrowing_list@borrowing_list, get_vars, character(1)))
    trt_cols <- unique(vapply(object@treatment_list@treatment_list, get_vars, character(1)))
    out_cols <- unique(unlist(lapply(object@outcome_list@outcome_list, get_vars)))

    c(
      cov_cols,
      ext_cols,
      trt_cols,
      out_cols
    )
  }
)

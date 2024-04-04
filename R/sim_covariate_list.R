#' `SimCovariateList` Class
#'
#' A class for covariate details as part of a simulation study.
#'  Objects of class `SimCovariateList` should not be created
#' directly but by the constructor `sim_covariate_list()`.
#'
#' @slot covariate_list named list of object of class `Covariate`, one object
#' for each parameter variation.
#'
#' @include covariate_class.R
#' @include add_covariates.R
#' @include analysis_class.R
.sim_covariate_list <- setClass(
  "SimCovariateList",
  slots = c(
    covariate_list = "list",
    guide = "data.frame"
  ),
  validity = function(object) {
    if (!all(vapply(object@covariate_list,
      function(item) is(item, "CovariatesOrNULL"),
      FUN.VALUE = logical(1)
    ))) {
      return("`covariate_list` must be a list of `Covariate` objects (or `NULL`)")
    }
    if (is.null(names(object@covariate_list))) {
      return("`covariate_list` must be named.")
    }
    if (any(names(object@covariate_list) == "")) {
      return("All items in `covariate_list` must be named.")
    }
    if (length(unique(names(object@covariate_list))) != length(names(object@covariate_list))) {
      return("All names supplied to `covariate_list` must be unique.")
    }
  }
)

#' Input covariate adjustment details for a simulation study
#'
#' A function for defining which covariate adjustment scenarios should be evaluated as
#' part of a simulation study.
#'
#' @param covariate_list named list of objects of class `Covariate` created
#' by `add_covariates()`.
#'
#' @details
#' This function allows the user to specify covariate adjustment details that will be included
#' as part of a simulation study. It is often of interest to compare several adjustment methods to
#' no adjustment. To specify no adjustment, pass `NULL` as a list item to `covariate_list`.
#'
#' @family simulation classes
#'
#' @return Object of class [`SimCovariateList`][SimCovariateList-class].
#'
#' @export
#'
#' @examples
#'
#' covariates <- sim_covariate_list(
#'   list(
#'     "No adjustment" = NULL,
#'     "Covariates 1 and 2" = add_covariates(c("cov1", "cov2"), prior_normal(0, 1000))
#'   )
#' )
#'
sim_covariate_list <- function(covariate_list) {
  covariate <- .sim_covariate_list(
    covariate_list = covariate_list
  )

  covariate@guide <- data.frame(
    covariate_scenario = names(covariate@covariate_list)
  )

  covariate
}

# show ----
setMethod(
  f = "show",
  signature = "SimCovariateList",
  definition = function(object) {
    cat("SimCovariateList object with ", NROW(object@covariate_list), " different scenario(s)\n")
    if (NROW(object@covariate_list) <= 10) {
      print(object@guide)
    }
  }
)


# get_vars ----
#' @rdname get_vars
#' @include generics.R
setMethod(
  f = "get_vars",
  signature = "SimCovariateList",
  definition = function(object) {
    cov_cols <- if (!is.null(object@covariate_list)) {
      unlist(lapply(object@covariate_list, get_vars))
    } else {
      NULL
    }
    names(cov_cols) <- NULL
    return(cov_cols)
  }
)

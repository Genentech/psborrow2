#' `SimCovariates` Class
#'
#' A class for specifying covariate distributions and covariance for
#' simulation studies.
#'
#' @slot covariates list. List of covariate mean values or probabilities as
#' generated through `bin_var()` (class `SimVarBin` or `cont_var()`
#' (class `SimVarCont`).
#' @slot covariance_internal matrix. Covariance matrix before binarization
#' for internal patients.
#' @slot covariance_external matrix. Covariance matrix before binarization
#' for external patients.
#'
.sim_covariates <- setClass(
  "SimCovariate",
  slots = c(
    covariates = "list",
    covariance_internal = "matrix",
    covariance_external = "matrix"
  ),
  validity = function(object) {

    # Covariates are named
    if (is.null(names(object@covariates)) |
        any(names(object@covariates) == "")) {
      return("All covariates must be named")
    }
    if (!all(sapply(object@covariates, is, "SimVar"))) {
      return("`covariates` must all be of class `SimVar` (use `bin_var()` or `cont_var()`)")
    }
    if (length(unique(names(object@covariates))) != length(names(object@covariates))) {
      return("named arguments to list for argument `covariates` must all be different")
    }

    # Covariance matrices are square, symmetric, correct length
    if (!all(dim(object@covariance_internal) == rep(length(object@covariates), 2)) ||
        !all(dim(object@covariance_external) == rep(length(object@covariates), 2)) ||
        !isSymmetric(object@covariance_internal) ||
        !isSymmetric(object@covariance_external)) {
      return(paste0("Covariance matrices must be symmetric square matrices width ",
                  "height and width equal to the number of covariates (",
                  length(object@covariates), ")."))
    }

    # Matrices are semi definite
    if (!matrixcalc::is.positive.semi.definite(object@covariance_internal) ||
        !matrixcalc::is.positive.semi.definite(object@covariance_external)) {
      return(paste0("Covariance matrices must be semi positive definite. ",
                  "Try using a different matrix or finding the nearest ",
                  "positive definite matrix (e.g., with `Matrix::nearPD()`"))
    }

  }

)

#' Specify covariates for simulation study
#'
#' Provide details on the desired covariate distributions and covariance for
#' for a simulation study.
#'
#' @param covariates list. Named list of covariate mean values or probabilities as
#' generated through `bin_var()` (class `SimVarBin` or `cont_var()`
#' (class `SimVarCont`). See `details` for more information.
#' @param covariance_internal matrix. Covariance matrix before binarization
#' for internal patients.
#' @param covariance_external matrix. Covariance matrix before binarization
#' for external patients.
#'
#' @return Object of class `SimCovariate`
#'
#' @details
#' This function is intended to specify the number of covariates and
#' relationships between them for the purposes of designing a simulation
#' study in `psborrow2`. Because the outcome model does not necessarily
#' need to adjust for covariates, this function is not necessary in
#' `create_simulation_obj()`. The relationship between the treatment
#' and the outcome is specified elsewhere (i.e, in `sim_survival()` or
#' `sim_binary_event()`).
#'
#' We need a few things to
#'
#' @export
#' @family simulation
#' @examples
#'
#' set.seed(123)
#' covmat <- matrix(rWishart(1, 2, diag(2)), ncol = 2)
#'
#' covset1 <- sim_covariates(
#'    covariates = list(cov1 = bin_var(0.5, 0.5),
#'                      cov2 = cont_var(100, 130)),
#'    covariance_internal = covmat,
#'    covariance_external = covmat
#')
sim_covariates <- function(covariates,
                           covariance_internal,
                           covariance_external) {
  # Check input classes
  assert_list(covariates)
  assert_matrix(covariance_internal)
  assert_matrix(covariance_external)

  # Construct object
  sim_covariates_obj <- .sim_covariates(
    covariates = covariates,
    covariance_internal = covariance_internal,
    covariance_external = covariance_external
  )

  # Return
  return(sim_covariates_obj)
}

# show ----
setMethod(
  f = "show",
  signature = "SimCovariate",
  definition = function(object) {
    cat("SimCovariate Object\n")
  }
)

#' `SimSampleSize` Class
#'
#' A class for creating matrices for simulation studies
#' containing flags specifying whether the patient is from the concurrent
#' trial or not (`ext` = 0 for concurrent trial, `ext` = 1 for historical
#' data) and whether the patient is on the experimental therapy or not
#' (`trt` = 0 for no experimental therapy, `trt` = 1 for experimental therapy).
#'
#' @slot n_internal_control integer. Number of patients to be simulated in the
#' internal control arm.
#' @slot n_external_control integer. Number of patients to be simulated in the
#' external control arm.
#' @slot n_internal_experimental integer. Number of patients to be simulated
#' in the internal experimental arm.
#' @slot mat matrix. Matrix with two columns, `ext` (flag for being from
#' external data source) and `trt` (flag for receiving experimental
#' treatment)
.sim_samplesize <- setClass(
  "SimSampleSize",
  slots = c(
    n_internal_control = "numeric",
    n_external_control = "numeric",
    n_internal_experimental = "numeric",
    mat = "matrix"
  ),
  validity = function(object) {
    if (object@n_internal_control <= 0) {
      return("n_internal_control must be >0")
    }
    if (object@n_external_control <= 0) {
      return("n_external_control must be >0")
    }
    if (object@n_internal_experimental <= 0) {
      return("n_external_experimental must be >0")
    }
  }
)

#' Set simulation study parameters for sample size
#'
#' @param n_internal_control integer. Number of patients to be simulated in the
#' internal control arm.
#' @param n_external_control integer. Number of patients to be simulated in the
#' external control arm.
#' @param n_internal_experimental integer. Number of patients to be simulated
#' in the internal experimental arm.
#'
#' @return Object of class `SimSampleSize`
#' @export
#' @family simulation
#' @examples
#' ss <- sim_samplesize(200, 200, 500)
sim_samplesize <- function(n_internal_control,
                           n_external_control,
                           n_internal_experimental) {
  assert_int(n_internal_control)
  assert_int(n_external_control)
  assert_int(n_internal_experimental)

  sim_samplesize_obj <- .sim_samplesize(
    n_internal_control = n_internal_control,
    n_external_control = n_external_control,
    n_internal_experimental = n_internal_experimental
  )

  ext <- rep(c(0L, 1L, 0L), times = c(n_internal_control, n_external_control, n_internal_experimental))
  trt <- rep(c(0L, 0L, 1L), times = c(n_internal_control, n_external_control, n_internal_experimental))
  sim_samplesize_obj@mat <- cbind(ext, trt)

  return(sim_samplesize_obj)
}

# show ----
setMethod(
  f = "show",
  signature = "SimSampleSize",
  definition = function(object) {
    cat("SimSampleSize Object\n")
    cat(glue::glue("{object@n_internal_control} internal control patients"))
    cat("\n")
    cat(glue::glue("{object@n_external_control} external control patients"))
    cat("\n")
    cat(glue::glue("{object@n_internal_experimental} external treated patients"))
  }
)

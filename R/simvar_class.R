#' `SimVar` Class
#'
#' A parent class for defining covariates to be created in the simulation study
#' calls to `add_covariates()`.
setClass(
  "SimVar",
  contains = "VIRTUAL"
)

#' `SimVarCont` class
#'
#' A constructor for making objects of class `SimVarCont`.
#' Objects of class `SimVarCont` are used to hold mean values of
#' of continuous variables specified in a simulation study.
#'
#' @slot mu_internal numeric. Mean covariate value for the internal arms.
#' @slot mu_external numeric. Mean covariate value for the external arm.
#' @slot printval_int numeric. Value to print to summarize internal arms.
#' @slot printval_ext numeric. Value to print to summarize external arm.
#' @slot type_string character. 'continuous'
#' @family simvar classes
.cont_var <- setClass(
  "SimVarCont",
  contains = "SimVar",
  slots = c(
    mu_internal = "numeric",
    mu_external = "numeric",
    printval_int = "numeric",
    printval_ext = "numeric",
    type_string = "character"
  ),
  prototype = list(
    type_string = "continuous"
  )
)

#' Create continuous covariate
#'
#' Create an object of class `SimVarCont` to hold mean values of
#' of continuous variables specified in a simulation study.
#'
#' @param mu_internal numeric. Mean covariate value for the internal arms.
#' @param mu_external numeric. Mean covariate value for the external arm.
#' @return Object of class [`SimVarCont`][SimVarCont-class].
#' @export
#' @family simvar
#' @examples
#' cv1 <- cont_var(0.5, 1)
#' cv2 <- cont_var(10, 10)
cont_var <- function(mu_internal,
                     mu_external) {
  expect_numeric(mu_internal)
  expect_numeric(mu_external)
  .cont_var(
    mu_internal = mu_internal,
    mu_external = mu_external,
    printval_int = mu_internal,
    printval_ext = mu_external
  )
}

setMethod(
  f = "show",
  signature = "SimVarCont",
  definition = function(object) {
    cat("Continuous variable\n")
    cat(glue::glue("internal mean: {object@mu_internal}, external mean: {object@mu_external}"))
  }
)

#' `SimVarBin` class
#'
#' A constructor for making objects of class `SimVarBin`.
#' Objects of class `SimVarBin` are used to hold proportions of
#' binary variables specified in a simulation study.
#'
#' @slot prob_internal numeric. Proportion for the internal arms.
#' @slot prob_external numeric. Proportion for the external arm.
#' @slot mu_internal_before_bin numeric. Mean value of covariate before binarization for
#' the internal arms.
#' @slot mu_external_before_bin numeric. Mean value of covariate before binarization for
#' the external arm.
#' @slot printval_int numeric. Value to print to summarize internal arms.
#' @slot printval_ext numeric. Value to print to summarize external arm.
#' @slot type_string character. 'binary'
#'
#' @family simvar classes
.bin_var <- setClass(
  "SimVarBin",
  contains = "SimVar",
  slots = c(
    prob_internal = "numeric",
    prob_external = "numeric",
    mu_internal_before_bin = "numeric",
    mu_external_before_bin = "numeric",
    printval_int = "numeric",
    printval_ext = "numeric",
    type_string = "character"
  ),
  validity = function(object) {
    if (object@prob_internal < 0 | object@prob_internal > 1) {
      return("`prob_internal` must be [0, 1]")
    }
    if (object@prob_external < 0 | object@prob_external > 1) {
      return("`prob_external` must be [0, 1]")
    }
  },
  prototype = list(
    type_string = "binary"
  )
)

#' Create binary covariate
#'
#' Create an object of class `SimVarBin` to hold proportions of binary
#' variables specified in a simulation study.
#'
#' @param prob_internal numeric. Proportion for the internal arms.
#' @param prob_external numeric. Proportion for the external arm.
#' @param mu_internal_before_bin numeric. Mean value of the covariate before binarization
#' for the internal arms. The default is 0. See `details` for more information.
#' @param mu_external_before_bin numeric. Mean value of the covariate before binarization
#' for the external arm. The default is 0. See `details` for more information.
#'
#' @details
#' This function contains information necessary to create binary covariates
#' as part of a simulation study. The binary covariates are created
#' by binarizing multivariate normal distributions to achieve
#' the probabilities specified in `prob_internal` and `prob_external`. The
#' user may choose to change the default mean value of each variable
#' prior to binarization by specifying `mu_internal_before_bin` or
#' `mu_external_before_bin` to ensure the correct scales are used in the
#' covariance matrix, though the ultimate proportions will depend on
#' `prob_internal` and `prob_external`. The default values for
#' `mu_internal_before_bin` and `mu_external_before_bin` are `0`, and
#' it is not recommended to change these without good reason.
#'
#' @return Object of class [`SimVarBin`][SimVarBin-class].
#'
#' @export
#' @family simvar
#' @examples
#' cv1 <- bin_var(0.50, 0.80)
#' cv2 <- bin_var(.95, .92)
bin_var <- function(prob_internal,
                    prob_external,
                    mu_internal_before_bin = 0,
                    mu_external_before_bin = 0) {
  expect_numeric(prob_internal)
  expect_numeric(prob_external)
  expect_numeric(mu_internal_before_bin)
  expect_numeric(mu_external_before_bin)
  .bin_var(
    prob_internal = prob_internal,
    prob_external = prob_external,
    mu_internal_before_bin = mu_internal_before_bin,
    mu_external_before_bin = mu_external_before_bin,
    printval_int = prob_internal,
    printval_ext = prob_external
  )
}

setMethod(
  f = "show",
  signature = "SimVarBin",
  definition = function(object) {
    cat("Binary variable\n")
    cat(glue::glue("internal probability: {object@prob_internal}, external probability: {object@prob_external}"))
  }
)

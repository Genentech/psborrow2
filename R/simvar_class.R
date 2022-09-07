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
#' @family simvar classes
.cont_var <- setClass(
  "SimVarCont",
  contains = "SimVar",
  slots = c(
    mu_internal = "numeric",
    mu_external = "numeric"
  )
)

#' Create continuous covariate
#'
#' Create an object of class `SimVarCont` to hold mean values of
#' of continuous variables specified in a simulation study.
#'
#' @param mu_internal numeric. Mean covariate value for the internal arms.
#' @param mu_external numeric. Mean covariate value for the external arm.
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
    mu_external = mu_external
  )
}

#' `SimVarBin` class
#'
#' A constructor for making objects of class `SimVarBin`.
#' Objects of class `SimVarBin` are used to hold proportions of
#' binary variables specified in a simulation study.
#'
#' @slot prob_internal numeric. Proportion for the internal arms.
#' @slot prob_external numeric. Proportion for the external arm.
#' @family simvar classes
.bin_var <- setClass(
  "SimVarBin",
  contains = "SimVar",
  slots = c(
    prob_internal = "numeric",
    prob_external = "numeric"
  ),
  validity = function(object) {
    if (object@prob_internal < 0 | object@prob_internal > 1) {
      return("`prob_internal` must be [0, 1]")
    }
    if (object@prob_external < 0 | object@prob_external > 1) {
      return("`prob_external` must be [0, 1]")
    }
  }
)

#' Create binary covariate
#'
#' Create an object of class `SimVarBin` to hold proportions of binary
#' variables specified in a simulation study.
#'
#' @param prob_internal numeric. Proportion for the internal arms.
#' @param prob_external numeric. Proportion for the external arm.
#' @export
#' @family simvar
#' @examples
#' cv1 <- bin_var(0.50, 0.80)
#' cv2 <- bin_var(.95, .92)
bin_var <- function(prob_internal,
                    prob_external) {
  expect_numeric(prob_internal)
  expect_numeric(prob_external)
  .bin_var(
    prob_internal = prob_internal,
    prob_external = prob_external
  )
}

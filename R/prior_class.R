#' `Prior` Class
#'
#' A class for defining priors to be translated to Stan code. Objects of class
#' `Prior` should not be created directly but by one of the specific prior
#' class constructors.
#'
#' @slot stan_code character. Stan implementation of the prior, with
#' placeholders for parameters surrounded with `{{` and `}}` to be replaced
#' with [glue::glue()].
#' @slot n_param integer. Number of prior parameters.
#' @family priors
#' @exportClass Prior
setClass(
   "Prior",
   slots = c(
      stan_code = "character",
      n_param = "integer"
   ),
   contains = "VIRTUAL"
)

# Print method
setMethod(
   f = "show",
   signature = "Prior",
   definition = function(object) {
      cat(
         class(object)[1],
         "object with parameters",
         glue::glue(object@stan_code, .open = "{{", .close = "}}")
      )
   }
)

#' # Combine class types
#' setClassUnion("Outcome_", c("WeibullPHSurvDist","ExponentialPrior"))
#'
#' # Outcome class
#' .outcome_class <- setClass(
#'    "Outcome",
#'    slots = c(time_var = "character",
#'              cens_var = "character",
#'              outcome_object = "Outcome_"),
#'    validity = function(object) {
#'       return(TRUE)
#'    }
#' )
#'
#' # Print method
#' setMethod(
#'    f = "show",
#'    signature = "Outcome",
#'    definition = function(object) {
#'       cat("Outcome object with censorship variable `", object@cens_var,
#'           "` and time variable `", object@time_var,"`")
#'    }
#' )
#'
#' #' Specify time and censorship columns in model matrix and specify survival
#' #' distribution for
#' #'
#' #' @param covariates character vector naming covariates to be adjusted for
#' #' @param priors either a single prior distribution applying to all covariates
#' #' or a named list of prior distributions, one for each covariate
#' #'
#' #' @return object of class "Covariates"
#' #' @export
#' #'
#' #' @examples
#' #' set_covariates(covariates = c('a','b'),
#' #'                priors = list('a' = normal_prior(0,1),
#' #'                              'b' = normal_prior(0,2)))
#' set_covariates <- function(covariates,
#'                            priors) {
#'
#'    # Additional errors not captured in class
#'    if (class(priors) != 'list' &&
#'        !is(priors, "Prior")) {
#'       stop("priors argument must be a single object of class `Prior`",
#'            " or a named list of objects of class `Prior`")
#'    }
#'
#'    # Create class
#'    .covariate_class(covariates = covariates,
#'                     priors = priors)
#' }

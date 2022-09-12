.sim_cov_list <- setClass(
  "SimCovariateList",
  slots = c(covariate_list = "list"),
  validity = function(object) {
    if (!all(vapply(object@outcome_list,
                    function(b) is(b, "Covariates"),
                    FUN.VALUE = logical(1)))) {
      return("`covariate_list` must be a list of `Covariate` objects.")
    }
  }
)

sim_cov_list <- function(covariate_list){

  .sim_cov_list(
    covariate_list = covariate_list
  )

}

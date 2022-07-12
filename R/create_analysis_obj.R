create_analysis_obj <- function(
   model_matrix,
   covariates = NULL,
   survival,
   borrowing,
   treatment_arms
) {

   # Verify input classes ----
   if (!"matrix" %in% class(model_matrix)) {
      stop("`model_matrix` must be a matrix")
   }
   if (!is.null(covariates)  && !is(covariate, "Covariate")) {
      stop("`covariates` should take the output of the function `set_covariates()`")
   }



}

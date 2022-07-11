create_analysis_obj <- function(
   model_matrix,
   covariate,
   survival,
   borrowing,
   treatment_arms
) {

   # Verify input classes ----
   if (!"matrix" %in% class(model_matrix)) {
      stop("`model_matrix` must be a matrix")
   }



}

create_analysis_obj <- function(
   model_matrix,
   covariates,
   outcome,
   borrowing,
   treatment_arms
) {

   # Input checks----
   ## Covariates----
   if(!is.null(covariates)) {
      if (!all(covariates@covariates %in% colnames(model_matrix))) {
         stop("Covariate columns are not all specified in model matrix")
      }
   }

   ## Outcomes----


}

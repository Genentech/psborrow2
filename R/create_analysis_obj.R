create_analysis_obj <- function(
   model_matrix,
   covariates = NULL,
   outcome,
   borrowing,
   treatment_arms,
   export_stan_model = FALSE,
   stan_model_path = "./model.stan"
) {

   # Input checks----
   ## Covariates----
   if (!is.null(covariates)) {
      if (!all(covariates@covariates %in% colnames(model_matrix))) {
         stop(paste0(
            "Covariate columns `",
            paste0(covariates@covariates[
               !covariates@covariates %in% colnames(model_matrix)
            ], collapse = "`, `"),
            "` are not in the model matrix"))
      }
   }

   ## Outcomes----
   if (!outcome@time_var %in% colnames(model_matrix)) {
      stop(paste0(
         "Time variable `",
         outcome@time_var,
         "` is not a column in the model matrix"))
   }
   if (!outcome@cens_var %in% colnames(model_matrix)) {
      stop(paste0(
         "Censor variable `",
         outcome@cens_var,
         "` is not a column in the model matrix"))
   }


   message("Inputs look good, time to make the STAN model")


}

create_analysis_obj <- function(
   model_matrix,
   covariates = NULL,
   outcome,
   borrowing,
   treatment_arms,
   export_stan_model = FALSE,
   stan_model_path = "./model.stan"
) {

   # Check columns exist----
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

   ## Treatment arms----
   if (!treatment_arms@ext_flag_col %in% colnames(model_matrix)) {
      stop(paste0(
         "External flag variable `",
         treatment_arms@ext_flag_col,
         "` is not a column in the model matrix"))
   }

   if (!treatment_arms@trt_flag_col %in% colnames(model_matrix)) {
      stop(paste0(
         "Treatment flag variable `",
         treatment_arms@trt_flag_col,
         "` is not a column in the model matrix"))
   }

   ## Select only relevant columns in model matrix----
   if (is(outcome,"TimeToEvent")) {
      mm <- model_matrix[,c(
         treatment_arms@trt_flag_col,
         treatment_arms@ext_flag_col,
         outcome@time_var,
         outcome@cens_var,
         covariates@covariates
      )]
   }

   # Check for correct data types and no missing values----
   if (!is.numeric(mm)) {
      stop("Model matrix must be numeric")
   }

   if (!all(complete.cases(mm))) {
      stop("There are missing values in the model matrix! Right now,
           no methods for missing data are supported")
   }

   # Make analysis object ----
   analysis_obj <- .analysis_obj(
      model_matrix = mm,
      covariates = covariates,
      outcome = outcome,
      borrowing = borrowing,
      treatment_arms = treatment_arms
   )

   message("\r", "Inputs look good", appendLF = FALSE)

   # Write the STAN script----
   message("\r", "Writing STAN program", appendLF = FALSE)

   ## Function string ----
   function_str <- glue::glue("
      functions {
         {{outcome@function_stan_code}}
      }", .open = "{{", .close = "}}")

   ## Data string ----
   data_str <- glue::glue("data {", .open = "{{", .close = "}}")

   ### Data shared by all TTE
   if (is(outcome,"TimeToEvent")) {
      data_str <- glue::glue("
         {{data_str}}
            int<lower=0> N;
            real time[N];
            int cens[N];
         ", .open = "{{", .close = "}}")
   }

   ### Add external control flag for BDB
   if (borrowing@method=="BDB") {
      data_str <- glue::glue("
                             {{data_str}}
                             matrix[N,2] Z;
                             ",
                             .open = "{{",
                             .close = "}}")
   }

   ### Add covariates for when these are specified
   if (!is.null(covariates)) {
      data_str <- glue::glue("
            {{data_str}}
            int<lower=0> K;
            matrix[N, K] X;
         ", .open = "{{", .close = "}}")
   }

   ### Close brackets
   data_str <- glue::glue("{{data_str}} }", .open = "{{", .close = "}}")

   ## Parameter string ----
   ### Set parameters shared by all methods
   param_str <- glue::glue("parameters {
                            real beta_trt;
                           ", .open = "{{", .close = "}}")

   ### Set tau and alpha[2] for BDB
   if (borrowing@method == "BDB") {
      param_str <- glue::glue("{{param_str}}
                              real<lower=0> tau;
                              vector[2] alpha;",
                              .open = "{{",
                              .close = "}}")
   }

   ### Add outcome specific parameters
   if (NROW(outcome@param_priors) > 0) {
      for (i in 1:NROW(outcome@param_priors)) {
         param_str <- glue::glue("{{param_str}}
                                 real {{names(outcome@param_priors[i])}} ;",
                                 .open = "{{",
                                 .close = "}}"
         )
      }
   }

   ### Add in vector of coefficients if covariates are provided
   if (!is.null(covariates)) {
      param_str <- glue::glue("{{param_str}}
                               vector[K] beta ;",
                              .open = "{{",
                              .close = "}}")
   }

   ### Close brackets
   param_str <- glue::glue("{{param_str}} }", .open = "{{", .close = "}}")

   ## Model string ----
   model_str <- glue::glue("model {
                           vector[N] lp;
                           ", .open = "{{", .close = "}}")

   if (!is.null(covariates)) {
      model_str <- glue::glue("{{model_str}}
                              lp = exp(X * beta + Z * alpha + beta_trt * trt );",
                              .open = "{{",
                              .close = "}}"
      )
   } else {
      model_str <- glue::glue("{{model_str}}
                              lp = exp(Z * alpha + beta_trt * trt );",
                              .open = "{{",
                              .close = "}}"
      )
   }

   if (NROW(outcome@param_priors) > 0) {
      for (i in 1:NROW(outcome@param_priors)) {
         name <- names(outcome@param_priors)[i]
         object <- outcome@param_priors[[name]]
         value <- glue::glue(object@stan_code, .open = "{{", .close = "}}")
         prior_str <- glue::glue("
            {{name}} ~ {{value}} ;", .open = "{{", .close = "}}")
         model_str <- glue::glue("
                                      {{model_str}}
                                      {{prior_str}}
                                      ", .open = "{{", .close = "}}")

      }
   }

   model_str <- glue::glue("
                           {{model_str}}
                           tau ~

                           ")

   if (is(outcome, "TimeToEvent")) {

   }
}

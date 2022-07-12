create_analysis_obj <- function(
   model_matrix,
   covariates = NULL,
   outcome,
   borrowing,
   treatment_arms
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

   ## Treatment and external control arms----
   if (borrowing@method == "BDB" && !borrowing@ext_flag_col %in% colnames(model_matrix)) {
      stop(paste0(
         "External flag variable `",
         borrowing@ext_flag_col,
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
         borrowing@ext_flag_col,
         outcome@time_var,
         outcome@cens_var,
         covariates@covariates
      )]
   }

   if (borrowing@method == "No borrowing") {
      mm <- mm[mm[,borrowing@ext_flag_col]==0,]
      if (sum(mm[,borrowing@ext_flag_col]==1) > 0) {
         warning("\r", paste0("Removing ",
                              sum(mm[,borrowing@ext_flag_col]==1),
                              " patients who were from the",
                              " external control cohort because borrowing",
                              " type is 'No borrowing'"))
      }
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
            vector[N] time;
            vector[N] cens;
            vector[N] trt;
         ", .open = "{{", .close = "}}")
   }

   ### Add external control flag for BDB
   if (borrowing@method=="BDB") {
      data_str <- glue::glue("
                             {{data_str}}
                             matrix[N,2] Z;
                             vector[N] ext;
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
   ### Set values shared by all
   object <- borrowing@ext_log_hazard_rate_prior
   beta_trt_prior <- glue::glue(object@stan_code, .open="{{", .close ="}}")
   model_str <- glue::glue("model {
                           vector[N] lp;
                           beta_trt ~ {{beta_trt_prior}} ;
                           ", .open = "{{", .close = "}}")

   ### Specify different combinations
   if (!is.null(covariates) && borrowing@method == "BDB") {
      model_str <- glue::glue("{{model_str}}
                              lp = exp(X * beta + Z * alpha + trt * beta_trt );",
                              .open = "{{",
                              .close = "}}"
      )
   } else if (is.null(covariates) && borrowing@method == "BDB") {
      model_str <- glue::glue("{{model_str}}
                              lp = exp(Z * alpha + trt * beta_trt);",
                              .open = "{{",
                              .close = "}}"
      )
   } else if (!is.null(covariates) && borrowing@method != "BDB") {
      model_str <- glue::glue("{{model_str}}
                              lp = exp(X * beta + trt * beta_trt );",
                              .open = "{{",
                              .close = "}}"
      )
   } else if (is.null(covariates) && borrowing@method != "BDB") {
      model_str <- glue::glue("{{model_str}}
                              lp = exp(trt * beta_trt );",
                              .open = "{{",
                              .close = "}}"
      )
   }

   ### Add priors for relevant parameters
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

   ### Add in tau and alphas if method = BDB
   if (borrowing@method == "BDB") {
      object <- borrowing@tau_prior
      tau_prior <- glue::glue(object@stan_code, .open = "{{", .close = "}}")

      object <- borrowing@ext_log_hazard_rate_prior
      alpha_2_prior <- glue::glue(object@stan_code, .open = "{{", .close = "}}")

      model_str <- glue::glue("
                           {{model_str}}
                           tau ~ {{tau_prior}} ;
                           real sigma;
                           sigma = 1 / tau;
                           alpha[2] ~ {{alpha_2_prior}} ;
                           alpha[1] ~ normal(alpha[2], sqrt(sigma)) ;
                           ", .open = "{{", .close = "}}")
   }

   ### Add in likelihood function
   model_str <- glue::glue("
                           {{model_str}}
                           {{outcome@likelihood_stan_code}}",
                           .open = "{{",
                           .close = "}}")

   ### Close brackets
   model_str <- glue::glue("{{model_str}} }", .open = "{{", .close = "}}")


   ## Combine model components ----
   model <- glue::glue("
                       {{function_str}}

                       {{data_str}}

                       {{param_str}}

                       {{model_str}}

                       ", .open = "{{", .close = "}}")

   # Write STAN model and compile ----
   stan_file <- cmdstanr::write_stan_file(model)
   stan_model <- cmdstanr::cmdstan_model(stan_file)
   message("\r", "STAN program compiled successfully", appendLF = FALSE)

   # Prepare data ----
   ## Common inputs
   data_in <- list(
      N = NROW(mm),
      trt = mm[,treatment_arms@trt_flag_col]
   )

   ## TTE additions
   if (is(outcome, "TimeToEvent")) {
      data_in[["time"]] <- mm[,outcome@time_var]
      data_in[["cens"]] <-  mm[,outcome@cens_var]
   }

   ## BDB additions
   if (borrowing@method == "BDB") {
      data_in[["ext"]] <- mm[,borrowing@ext_flag_col]
   }

   ## Covariate additions
   if (!is.null(covariates)) {

   }

   ##



}

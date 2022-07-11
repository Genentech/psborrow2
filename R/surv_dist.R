# Parent class
setClass(
   "SurvDist",
   slots = c(function_stan_code = "character",
             param_stan_code = "character",
             model_stan_code = "character",
             n_param = "integer",
             param_priors = "list"
             ),
   prototype = list(
      n_param = 0L,
      function_stan_code = "",
      param_stan_code = "",
      model_stan_code = ""
   ),
   contains = "VIRTUAL"
)

# Print method
setMethod(
   f = "show",
   signature = "SurvDist",
   definition = function(object) {
      cat(class(object)[1], "object")
   }
)

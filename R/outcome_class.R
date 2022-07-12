# Outcome class
setClass(
   "Outcome",
   contains = "VIRTUAL"
)

# TimeToEvent class
setClass(
   "TimeToEvent",
   slots = c(function_stan_code = "character",
             param_stan_code = "character",
             likelihood_stan_code = "character",
             n_param = "integer",
             param_priors = "list",
             time_var = "character",
             cens_var = "character"
             ),
   prototype = list(
      n_param = 0L,
      function_stan_code = "",
      param_stan_code = "",
      likelihood_stan_code = ""
   ),
   contains = "Outcome"
)

# BinaryEndpoint class
setClass(
   "BinaryEndpoint",
   slots = c(param_stan_code = "character",
             likelihood_stan_code = "character",
             outcome_var = "character"
   ),
   prototype = list(
      param_stan_code = "",
      likelihood_stan_code = ""
   ),
   contains = "Outcome"
)

# Print method
setMethod(
   f = "show",
   signature = "Outcome",
   definition = function(object) {
      cat("Outcome object with class ", class(object)[1])
   }
)

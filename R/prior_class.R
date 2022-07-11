# Parent class
setClass(
   "Prior",
   slots = c(stan_code = "character",
             n_param = "integer"),
   contains = "VIRTUAL"
)

# Print method
setMethod(
   f = "show",
   signature = "Prior",
   definition = function(object) {
      cat(class(object)[1],
          "object with parameters",
          glue::glue(object@stan_code, .open = "{{", .close = "}}"))
   }
)

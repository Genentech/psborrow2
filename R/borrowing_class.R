#' @include prior_class.R

setClassUnion("PriorOrNULL", c("Prior","NULL"))

# Parent class
.borrowing_class <- setClass(
   "Borrowing",
   slots = c(method = "character",
             tau_prior = "PriorOrNULL"),
   prototype = c(method = "No borrowing",
                 tau_prior = NULL),
   validity = function(object) {
      if (!object@method %in% c(
         "BDB",
         "Full borrowing",
         "No borrowing")) {
         return("method must be within ('BDB', 'Full borrowing', 'No borrowing')")
      }
      if (object@method != "BDB" && !is.null(object@tau_prior)) {
         return("no need to specify tau prior when method is not BDB")
      }
      return(TRUE)
   }
)

# Print method
setMethod(
   f = "show",
   signature = "Borrowing",
   definition = function(object) {
      cat("Borrowing class: ",
          object@method)
   }
)

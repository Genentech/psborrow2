.sim_outcome_list <- setClass(
  "SimOutcomeList",
  slots = c(outcome_list = "list"),
  validity = function(object) {
    if (!all(vapply(object@outcome_list,
                    function(b) is(b, "Outcome"),
                    FUN.VALUE = logical(1)))) {
      return("`outcome_list` must be a list of `Outcome` objects.")
    }
  }
)

sim_outcome_list <- function(outcome_list){

  .sim_outcome_list(
    outcome_list = outcome_list
  )

}

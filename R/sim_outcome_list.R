.sim_outcome_list <- setClass(
  "SimOutcomeList",
  slots = c(outcome_list = "list",
            guide = "data.frame"),
  validity = function(object) {
    if (!all(vapply(object@outcome_list,
                    function(b) is(b, "Outcome"),
                    FUN.VALUE = logical(1)))) {
      return("`outcome_list` must be a list of `Outcome` objects.")
    }
  }
)

sim_outcome_list <- function(outcome_list){

  outcome <- .sim_outcome_list(
    outcome_list = outcome_list
  )

  # Come up with nice print method at the outcome class level
  outcome@guide <- data.frame(
    outcome_scenario = rep(1:NROW(outcome_list))
  )

  outcome

}

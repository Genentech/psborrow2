make_model_string_transf_data <- function(object) {
  if (.hasSlot(object@outcome, "transformed_data_stan_code")) {
    str <- object@outcome@transformed_data_stan_code
    h_glue(
      "transformed data {
  {{str}}
}"
    )
  } else {
    ""
  }
}

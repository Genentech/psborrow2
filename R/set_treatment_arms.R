#' Specify treatment arm column, external control arm column, and set
#' priors for treatment effects and external control baseline hazard
#'
#' @param ext_flag_col name of the column in the model matrix which contains
#' flags (1/0 or T/F) for external controls (1 = external, 0 = internal)
#' @param trt_flag_col name of the column in the model matrix which contains
#' flags (1/0 or T/F) for experimental treatment (1 = experimental treatment,
#' 0 = control treatment)
#' @param trt_log_hazard_ratio_prior object of class `Prior` specifying the
#' prior distribution of the log hazard ratio comparing experimental to control
#' treatments.
#' @param ext_log_hazard_rate_prior object of class `Prior` specifying the prior
#' distribution for the log hazard rate for the concurrent control arm.
#'
#' @return an object of class `Treatment`
#' @export
#'
#' @include treatment_class.R
#'
#' @examples
#'
#' sta <- set_treatment_arms(
#'    ext_flag_col = 'ext',
#'    trt_flag_col = 'trt',
#'    trt_log_hazard_ratio_prior = normal_prior(0, 10000),
#'    ext_log_hazard_rate_prior = normal_prior(0, 10000)
#' )
set_treatment_arms <- function(
      ext_flag_col = "character",
      trt_flag_col = "character",
      trt_log_hazard_ratio_prior = "Prior",
      ext_log_hazard_rate_prior = "Prior"
) {

   .treatment_class(ext_flag_col = ext_flag_col,
                    trt_flag_col = trt_flag_col,
                    trt_log_hazard_ratio_prior = trt_log_hazard_ratio_prior,
                    ext_log_hazard_rate_prior = ext_log_hazard_rate_prior
   )

}

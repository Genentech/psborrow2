#' Cast data to "long" format for PEM
#'
#' Take an `Analysis` object's `data_matrix` and convert it to "long" format, one
#' row per patient per period
#'
#' @param analysis_obj `Analysis`. An object of class `Analysis`
#' as created through `.create_analysis_obj()`.
#'
#' @return An updated analysis object with the data matrix in "long" format
#' @noRd
#' @examples
#'
#' anls <- create_analysis_obj(
#'   data_matrix = example_matrix,
#'   covariates = add_covariates(
#'     covariates = c("cov1", "cov2"),
#'     priors = prior_normal(0, 1000)
#'   ),
#'   outcome = outcome_surv_pem(
#'     "time",
#'     "cnsr",
#'     baseline_prior = prior_normal(0, 1000),
#'     cut_points = c(1,2,3)
#'   ),
#'   borrowing = borrowing_hierarchical_commensurate(
#'     "ext",
#'     prior_exponential(.001)
#'   ),
#'   treatment = treatment_details(
#'     "trt",
#'     prior_normal(0, 1000)
#'   )
#' )
#'
#' anls_obj <- psborrow2:::cast_mat_to_long_pem(anls)
#'
cast_mat_to_long_pem <- function(analysis_obj) {

  ## Start with data.frame
  df <- as.data.frame(analysis_obj@data_matrix)
  cn <- colnames(df)

  ## Check cut points
  cut_points <- analysis_obj@outcome@cut_points
  max_fup <- max(df[,analysis_obj@outcome@time_var])
  cut_points_keep <- cut_points[cut_points < max_fup]
  if (length(cut_points_keep) < length(cut_points)) {
    warning(paste0("Some cut points are greater than the maximum follow-up time of ", max_fup, ". These will be ignored."))
  }

  ## Did they use a protected name?
  if (any(c("psb2__period", "psb2__start", "psb2__status", "__period__") %in% cn)) {
    stop("The column names 'psb2__period', 'psb2__status', 'psb2__start', and '__period__' are reserved when using PEM. Please rename your columns.")
  }

  ## Censorship flag -> event flag
  df$psb2__status <- 1 - df[, analysis_obj@outcome@cens_var]

  ## Create long data
  long_df <- survival::survSplit(data = df,
                                 cut = cut_points_keep,
                                 event = "psb2__status",
                                 episode = "psb2__period",
                                 start = "psb2__tstart",
                                 end = analysis_obj@outcome@time_var)
  names(long_df)[which(names(long_df) == "psb2__period")] <- "__period__"
  long_df[, analysis_obj@outcome@cens_var] <- 1 - long_df[, "psb2__status"]
  long_df <- long_df[, c(cn, "__period__")]
  long_mat <- as.matrix(long_df)

  # Update and return
  analysis_obj@data_matrix <- long_mat
  analysis_obj@outcome@n_periods <- as.integer(length(cut_points_keep) + 2)
  return(analysis_obj)

}

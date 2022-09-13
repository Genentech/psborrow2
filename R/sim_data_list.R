#' `SimDataList` Class
#'
#' A class for defining generated data for use as part of a
#' simulation study. Objects of class `SimDataList` should not be created
#' directly but by the constructor `sim_data_list()`.
#'
#' @slot data_list list of lists of matrices. The lists at the highest
#' level differ in that the parameters used to generate the data. The matrices
#' at lowest level are different iterations of the same data generation
#' parameters.
#' @slot guide data.frame. `guide` contains information on the parameters
#' that differ at the highest level of `data_list`.
#' @slot effect character. The column in each matrix within `data_list` that
#' corresponds to the true effect estimate (hazard ratio or odds ratio).
#' @slot drift character. The column in each matrix within `data_list` that
#' corresponds to the drift between external and internal control arms. A
#' drift >1 means the external arm experiences greater effects.
.sim_data_list <- setClass(
  "SimDataList",
  slots = c(data_list = "list",
            guide = "data.frame",
            effect = "character",
            drift = "character"
  ),
  validity = function(object) {
    if (NROW(object@guide) != NROW(object@data_list)) {
      return("`guide` and `data_list` must be same length")
    }
  }
)

#' Input generated data for a simulation study
#'
#' A class for defining generated data for use as part of a
#' simulation study.
#'
#' @param data_list list of lists of matrices. The lists at the highest
#' level differ in that the parameters used to generate the data. The matrices
#' at lowest level are different iterations of the same data generation
#' parameters. See `details`.
#' @param guide data.frame. `guide` contains information on the parameters
#' that differ at the highest level of `data_list`. See `details.`
#' @param effect character. The column in each matrix within `data_list` that
#' corresponds to the true effect estimate (hazard ratio or odds ratio).
#' @param drift character.
#'
#' @details
#'
#' In this function, you are providing generated data for analysis in a
#' simulation study in `psborrow2`. Note that this function does not
#' do any data generation on your behalf; it assumes that you have generated
#' the data already. For a full working example, refer to the relevant vignette:
#' `vignette('simulation_study', package = 'psborrow2')`.
#'
#' More information on the inputs is provided below.
#'
#' ## Matrix requirements in `data_list`
#'
#' Each matrix embedded in `data_list` must have:
#' 1) a flag for whether the patient is an external control
#' 2) a flag for whether the patient is in the experimental treatment arm
#' 3) outcome information (time and censorship for survival, flag for
#' outcome in binary endpoints)
#'
#' Optionally, the matrices may also contain covariates. See `examples`.
#'
#' ## `data_list`
#'
#' Each set of distinct data generation parameters should be represented by
#' a single list of matrices. Because multiple scenarios may want to be
#' compared, a list of list of matrices is preferred. See `examples`.
#'
#' ## `guide`
#'
#' The `guide` should be a data.frame with one row per scenario. As a
#' consquence of this, the length of the list should equal the number of rows
#' in the guide. See `examples`.
#'
#' @examples
#'
#' base_mat <- matrix(
#'    c(rep(0, 200), rep(0, 200), rep(1, 200),
#'    rep(1, 200), rep(0, 200), rep(0, 200),
#'    rep(0, 600)),
#'    ncol = 3,
#'    dimnames = list(NULL, c('ext','trt', 'driftOR'))
#' )
#'
#' add_binary_endpoint <- function(odds_ratio,
#'                                 base_matrix = base_mat) {
#'
#'   linear_predictor = base_matrix[,'trt'] * log(odds_ratio)
#'   prob = 1 / (1 + exp(-linear_predictor))
#'
#'   bin_endpoint = rbinom(NROW(base_matrix),
#'                         1,
#'                         prob)
#'
#'   cbind(base_matrix, matrix(bin_endpoint, ncol=1, dimnames = list(NULL, 'ep')))
#'
#' }
#'
#' set.seed(123)
#'
#' data_list = list(
#'    list(add_binary_endpoint(1.5), add_binary_endpoint(1.5)),
#'    list(add_binary_endpoint(2.5), add_binary_endpoint(2.5))
#' )
#'
#' guide <- data.frame(
#'    trueOR = c(1.5, 2.5),
#'    driftOR = c(1, 1)
#' )
#'
#' sdl <- sim_data_list(
#'    data_list = data_list,
#'    guide = guide,
#'    effect = 'trueOR',
#'    drift = 'driftOR
#' )
#'
#'
sim_data_list <- function(data_list,
                          guide,
                          effect,
                          drift) {

  .sim_data_list(
    data_list = data_list,
    guide = guide,
    effect = effect,
    drift = drift
  )

}

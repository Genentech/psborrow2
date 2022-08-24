#' Example data matrix
#'
#' A matrix containing data from a clinical trial with a treatment arm and a control arm,
#' as well as data from an external control.
#'
#' @format A data frame with 50 rows and 6 variables:
#' \describe{
#'   \item{ext}{0/1, flag for external controls}
#'   \item{trt}{0/1, flag for treatment arm}
#'   \item{cov1}{0/1, baseline covariate}
#'   \item{cov2}{0/1, baseline covariate}
#'   \item{time}{numeric >0, survival time}
#'   \item{cnsr}{0/1, censoring indicator}
#' }
"example_matrix"

#' Example data matrix
#'
#' A matrix containing data from a clinical trial with a treatment arm and a
#' control arm, as well as data from an external control. In this simulated
#' dataset, the true hazard ratio (HR) for the time-to-event endpoint
#' comparing the experimental treatment to the control treatment
#' is 0.70. The true odds ratio (OR) for the binary response endpoint
#' comparing the experimental treatment to the control treatment
#' is 1.20.
#'
#' @format A data frame with 500 rows and 11 columns. The distributions of
#' patients is: 50 internal control patients, 100 internal experimental
#' patients, 350 external control patients.
#'
#' \describe{
#'   \item{id}{patient identifier}
#'   \item{ext}{0/1, flag for external controls}
#'   \item{trt}{0/1, flag for treatment arm}
#'   \item{cov1}{0/1, baseline covariate}
#'   \item{cov2}{0/1, baseline covariate}
#'   \item{cov3}{0/1, baseline covariate}
#'   \item{cov4}{0/1, baseline covariate}
#'   \item{time}{numeric >0, survival time}
#'   \item{status}{0/1, indicator for event status
#'   (1 = had event, 0 = did not have event)}
#'   \item{cnsr}{0/1, censoring indicator
#'   (1 = was censored, 0 = was not censored)}. This value is 1 - status.
#'   \item{resp}{0/1, indicator for response outcome
#'   (1 = had a response, 0 = did not have a response)}
#' }
"example_matrix"

#' Simulated Survival Data
#'
#' A data frame containing simulated data from a clinical trial
#' with a treatment arm (n=200) and a control arm (n=158), as well as data
#' from an external control (n=242).
#'
#' @format A data frame with 600 rows and 6 variables:
#' \describe{
#'   \item{trt}{0/1, flag for treatment arm}
#'   \item{ext}{0/1, flag for external controls}
#'   \item{eventtime}{numeric >0, survival time}
#'   \item{status}{0/1, event indicator}
#'   \item{censor}{0/1, censoring indicator}
#'   \item{cov1}{0/1, binary baseline covariate 1}
#'   \item{cov2}{integer in \[0, 15\], baseline covariate 2}
#'   \item{cov3}{continuous numeric, baseline covariate 3}
#' }
"example_surv"

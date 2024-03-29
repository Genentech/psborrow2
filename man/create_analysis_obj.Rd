% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_analysis_obj.R
\name{create_analysis_obj}
\alias{create_analysis_obj}
\title{Compile MCMC sampler using STAN and create analysis object}
\usage{
create_analysis_obj(
  data_matrix,
  outcome,
  borrowing,
  treatment,
  covariates = NULL,
  quiet = FALSE
)
}
\arguments{
\item{data_matrix}{matrix. The data matrix, including all covariates to be
adjusted for, all relevant outcome variables, and treatment arm and external
control arm flags.}

\item{outcome}{\code{Outcome}. Object of class \code{\link[=Outcome-class]{Outcome}} as output by
\code{\link[=outcome_surv_exponential]{outcome_surv_exponential()}}, \code{\link[=outcome_surv_weibull_ph]{outcome_surv_weibull_ph()}}, or \code{\link[=outcome_bin_logistic]{outcome_bin_logistic()}}.}

\item{borrowing}{\code{Borrowing}. Object of class \code{\link[=Borrowing-class]{Borrowing}} as output by
\code{\link[=borrowing_full]{borrowing_full()}}, \code{\link[=borrowing_none]{borrowing_none()}}, and \code{\link[=borrowing_hierarchical_commensurate]{borrowing_hierarchical_commensurate()}}.}

\item{treatment}{\code{Treatment}. Object of class \code{\link[=Treatment-class]{Treatment}} as output by
\code{\link[=treatment_details]{treatment_details()}}.}

\item{covariates}{\code{Covariates}. Object of class \code{\link[=Covariates-class]{Covariates}} as output by
the function \code{\link[=add_covariates]{add_covariates()}}.}

\item{quiet}{logical. Whether to suppress messages (\code{TRUE}) or not (\code{FALSE},
the default)}
}
\value{
Object of class \code{\link[=Analysis-class]{Analysis}}.
}
\description{
Compile MCMC sampler using STAN and create analysis object
}
\examples{
if (check_cmdstan()) {
  anls <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_surv_exponential(
      "time",
      "cnsr",
      baseline_prior = prior_normal(0, 1000)
    ),
    borrowing = borrowing_hierarchical_commensurate(
      "ext",
      prior_exponential(.001)
    ),
    treatment = treatment_details(
      "trt",
      prior_normal(0, 1000)
    ),
    covariates = add_covariates(
      covariates = c("cov1", "cov2"),
      priors = prior_normal(0, 1000)
    )
  )
}

}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prior_half_cauchy.R
\docType{class}
\name{PriorHalfCauchy-class}
\alias{PriorHalfCauchy-class}
\alias{.prior_half_cauchy}
\title{\code{PriorHalfCauchy} Class}
\description{
A class for defining half cauchy priors to be translated to Stan code.
Objects of class \code{PriorHalfCauchy} should not be created directly but by
the constructor \code{\link[=prior_half_cauchy]{prior_half_cauchy()}}.
}
\section{Slots}{

\describe{
\item{\code{stan_code}}{character. Stan implementation of the prior, with
placeholders for the half cauchy stan function parameters surrounded with
\verb{\{\{} and \verb{\}\}} to be replaced with \code{\link[glue:glue]{glue::glue()}}.}

\item{\code{n_param}}{integer. Number of prior parameters (2).}

\item{\code{constraint}}{character. Support of prior distribution. In a half cauchy
prior, constraint is \code{mu}}

\item{\code{mu}}{numeric. Location.}

\item{\code{sigma}}{numeric. Scale (>0).}
}}

\seealso{
Other prior classes: 
\code{\link{Prior-class}},
\code{\link{PriorBernoulli-class}},
\code{\link{PriorBeta-class}},
\code{\link{PriorCauchy-class}},
\code{\link{PriorExponential-class}},
\code{\link{PriorGamma-class}},
\code{\link{PriorHalfNormal-class}},
\code{\link{PriorNormal-class}},
\code{\link{PriorPoisson-class}},
\code{\link{UniformPrior-class}}
}
\concept{prior classes}

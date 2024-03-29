% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{get_prior_string}
\alias{get_prior_string}
\title{Get Stan code for a \code{Prior}}
\usage{
get_prior_string(object)
}
\arguments{
\item{object}{\code{Prior} or list of \code{Prior} objects.}
}
\value{
A string containing the Stan code sampling from specified distribution.
}
\description{
Get Stan code for a \code{Prior}
}
\examples{
psborrow2:::get_prior_string(prior_normal(0, 100))
}

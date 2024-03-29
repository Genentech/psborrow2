% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_covariates.R
\docType{class}
\name{SimCovariates-class}
\alias{SimCovariates-class}
\alias{.sim_covariates}
\title{\code{SimCovariates} Class}
\description{
A class for specifying covariate distributions and covariance for
simulation studies.
}
\section{Slots}{

\describe{
\item{\code{covariates}}{list. List of covariate mean values or probabilities as
generated through \code{bin_var()} (class \code{SimVarBin} or \code{cont_var()}
(class \code{SimVarCont}).}

\item{\code{covariance_internal}}{matrix. Covariance matrix before binarization
for internal patients.}

\item{\code{covariance_external}}{matrix. Covariance matrix before binarization
for external patients.}
}}


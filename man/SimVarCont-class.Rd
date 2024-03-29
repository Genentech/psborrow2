% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simvar_class.R
\docType{class}
\name{SimVarCont-class}
\alias{SimVarCont-class}
\alias{.cont_var}
\title{\code{SimVarCont} class}
\description{
A constructor for making objects of class \code{SimVarCont}.
Objects of class \code{SimVarCont} are used to hold mean values of
of continuous variables specified in a simulation study.
}
\section{Slots}{

\describe{
\item{\code{mu_internal}}{numeric. Mean covariate value for the internal arms.}

\item{\code{mu_external}}{numeric. Mean covariate value for the external arm.}

\item{\code{printval_int}}{numeric. Value to print to summarize internal arms.}

\item{\code{printval_ext}}{numeric. Value to print to summarize external arm.}

\item{\code{type_string}}{character. 'continuous'}
}}

\seealso{
Other simvar classes: 
\code{\link{SimVarBin-class}}
}
\concept{simvar classes}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_data.R
\name{set_cut_off}
\alias{set_cut_off}
\title{Set Clinical Cut Off Rule}
\usage{
set_cut_off(object, internal = cut_off_none(), external = cut_off_none())
}
\arguments{
\item{object}{\code{DataSimObject}}

\item{internal}{\code{DataSimCutOff} object specified by one of the cut off functions: \code{cut_off_after_events()},
\code{cut_off_after_first()}, \code{cut_off_after_last()}, \code{cut_off_none()}.}

\item{external}{\code{DataSimCutOff} for the external data.}
}
\value{
A \code{DataSimObject} with updated \code{cut_off_internal} and \code{cut_off_external} slots.
}
\description{
Set Clinical Cut Off Rule
}
\examples{
data_sim <- create_data_simulation(
  create_baseline_object(10, 10, 10),
  event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 36)
)
set_cut_off(
  data_sim,
  cut_off_after_events(n = 10),
  cut_off_after_first(time = 30)
)
}

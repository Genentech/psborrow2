% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prior_beta.R
\name{prior_beta}
\alias{prior_beta}
\title{Prior beta distribution}
\usage{
prior_beta(alpha, beta)
}
\arguments{
\item{alpha}{numeric. Shape (>=0).}

\item{beta}{numeric. Shape (>=0).}
}
\value{
Object of class \code{\link[=PriorBeta-class]{PriorBeta}}
}
\description{
Prior beta distribution
}
\details{
Stan reference \url{https://mc-stan.org/docs/functions-reference/beta-distribution.html}
}
\examples{
bp <- prior_beta(9, 235)
}
\seealso{
Other priors: 
\code{\link{prior_bernoulli}()},
\code{\link{prior_cauchy}()},
\code{\link{prior_exponential}()},
\code{\link{prior_gamma}()},
\code{\link{prior_half_cauchy}()},
\code{\link{prior_half_normal}()},
\code{\link{prior_normal}()},
\code{\link{prior_poisson}()},
\code{\link{uniform_prior}()}
}
\concept{priors}

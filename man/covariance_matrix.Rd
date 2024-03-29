% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_data_baseline.R
\name{covariance_matrix}
\alias{covariance_matrix}
\title{Create Covariance Matrix}
\usage{
covariance_matrix(diag, upper_tri)
}
\arguments{
\item{diag}{Diagonal entries of the covariance matrix}

\item{upper_tri}{Upper triangle entries of the matrix, specified column wise.}
}
\value{
A symmetric matrix with \code{diag} values on the main diagonal and
\code{upper_tri} values in the lower and upper triangles.
}
\description{
Create Covariance Matrix
}
\examples{
m1 <- covariance_matrix(c(1, 1, 1, 1), c(.8, .3, .8, 0, 0, 0))
m1
mvtnorm::rmvnorm(5, mean = c(0, 0, 0, 0), sigma = m1)

# No correlation
covariance_matrix(c(1, 2, 3))

}

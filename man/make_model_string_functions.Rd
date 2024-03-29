% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_model_string_functions.R
\name{make_model_string_functions}
\alias{make_model_string_functions}
\title{Make model string of Stan's functions model bloc}
\usage{
make_model_string_functions(analysis_obj)
}
\arguments{
\item{analysis_obj}{\code{Analysis}. Object of class \code{\link[=Analysis-class]{Analysis}} created by
\code{psborrow2:::.analysis_obj()}.}
}
\value{
\code{glue} \code{character} containing the text for the functions block.
}
\description{
Create the Stan string encompassed by functions \code{{}}
}
\examples{
anls_obj <- psborrow2:::.analysis_obj(
  data_matrix = example_matrix,
  outcome = outcome_surv_exponential("time", "cnsr", prior_normal(0, 100)),
  borrowing = borrowing_full("ext"),
  treatment = treatment_details("trt", prior_normal(0, 100))
)

psborrow2:::make_model_string_functions(anls_obj)

}

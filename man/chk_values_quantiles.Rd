% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chk-values.R, R/err-values.R
\name{chk_values_quantiles}
\alias{chk_values_quantiles}
\alias{err_values_quantiles}
\title{Check values for Quantiles labels}
\usage{
chk_values_quantiles(x, name)

err_values_quantiles(x, name)
}
\arguments{
\item{x}{A character vector.}

\item{name}{The name for \code{x} that
will be used in error messages.}
}
\description{
Checks:
\itemize{
  \item is character
  \item no duplicates
  \item all elements have format "<number>%" where 0 <= number <= 100
}
}
\examples{
x <- c("2.5\%", "50\%", "97.5\%")
chk_values_quantiles(x = x, name = "x")
}

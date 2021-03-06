% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/err_tdy.R
\name{err_tdy_date_scalar}
\alias{err_tdy_date_scalar}
\alias{err_tdy_date_vector}
\title{Check and tidy a date scalar or vector}
\usage{
err_tdy_date_scalar(x, name)

err_tdy_date_vector(x, name)
}
\arguments{
\item{x}{A scalar or vector of class \code{\link[base:Dates]{Date}},
or something that can be coerced to one.}

\item{name}{The name for \code{x} that
will be used in error messages.}
}
\description{
Check and tidy a date scalar or vector
}
\examples{
x <- "2001-03-01"
err_tdy_date_scalar(x, name = "x")
x <- c("2000-05-01", "2000-06-01", "2000-07-01")
err_tdy_date_vector(x, name = "x")
}

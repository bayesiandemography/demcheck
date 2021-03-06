% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chk-single.R, R/err-single.R
\name{chk_is_numeric}
\alias{chk_is_numeric}
\alias{err_is_numeric}
\title{Check that a scalar or vector has type numeric}
\usage{
chk_is_numeric(x, name)

err_is_numeric(x, name)
}
\arguments{
\item{x}{A scalar or a vector.}

\item{name}{The name for \code{x} that
will be used in error messages.}
}
\description{
Checking type, e.g. fails for 1L.
}
\examples{
x <- 1.0
chk_is_numeric(x, name = "x")
}
\seealso{
\code{\link{chk_is_integer}} to test for
integer type.
}

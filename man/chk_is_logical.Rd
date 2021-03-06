% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chk-single.R, R/err-single.R
\name{chk_is_logical}
\alias{chk_is_logical}
\alias{err_is_logical}
\title{Check that a vector has type "logical"}
\usage{
chk_is_logical(x, name)

err_is_logical(x, name)
}
\arguments{
\item{x}{A vector.}

\item{name}{The name for \code{x} that
will be used in error messages.}
}
\description{
Check that a vector has type "logical"
}
\examples{
x <- c(TRUE, FALSE, NA)
chk_is_logical(x, name = "x")
}

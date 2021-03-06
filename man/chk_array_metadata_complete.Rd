% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chk-composite.R, R/err-composite.R
\name{chk_array_metadata_complete}
\alias{chk_array_metadata_complete}
\alias{err_array_metadata_complete}
\title{Check that an array has complete metadata}
\usage{
chk_array_metadata_complete(x, name)

err_array_metadata_complete(x, name)
}
\arguments{
\item{x}{An array.}

\item{name}{The name for \code{x} that
will be used in error messages.}
}
\description{
Check it is possible to uniquely identify every cell
in an array, using the array's dimnames and the
the names of the dimnames.
}
\examples{
x <- array(1:6,
           dim = 3:2,
           dimnames = list(region = c("a", "b", "c"),
                           sex = c("Female", "Male")))
chk_array_metadata_complete(x, name = "x")
}

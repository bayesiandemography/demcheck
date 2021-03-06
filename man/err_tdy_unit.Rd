% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/err_tdy.R
\name{err_tdy_unit}
\alias{err_tdy_unit}
\title{Check and tidy time unit}
\usage{
err_tdy_unit(x, name)
}
\arguments{
\item{x}{A string.}

\item{name}{The name for \code{x} that
will be used in error messages.}
}
\description{
Check and tidy time unit
}
\examples{
x <- "5 years"
err_tdy_unit(x, name = "x")
}
\seealso{
\code{\link{chk_member_unit}}
}

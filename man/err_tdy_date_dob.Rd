% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/err_tdy.R
\name{err_tdy_date_dob}
\alias{err_tdy_date_dob}
\title{Check and tidy dates and birth dates}
\usage{
err_tdy_date_dob(date, dob)
}
\arguments{
\item{date}{Dates when measurements made or events occurred.
A scalar or vector of class \code{\link[base:Dates]{Date}},
or something that can be coerced to one.}

\item{dob}{Dates of birth.
A scalar or vector of class \code{\link[base:Dates]{Date}},
or something that can be coerced to one. Must have
same length as \code{date}.}
}
\description{
Check and tidy dates and birth dates
}
\examples{
date <- c("2000-05-01", "2000-06-01", "2000-07-01")
dob <- c("1999-03-21", "1980-12-22", "1990-04-23")
err_tdy_date_dob(date = date, dob = dob)
}

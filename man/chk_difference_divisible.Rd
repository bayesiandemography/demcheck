% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chk-composite.R, R/err-composite.R
\name{chk_difference_divisible}
\alias{chk_difference_divisible}
\alias{err_difference_divisible}
\title{Check that differences between two variables
is divisible by a third variable}
\usage{
chk_difference_divisible(x1, x2, y, name1, name2, name_y)

err_difference_divisible(x1, x2, y, name1, name2, name_y)
}
\arguments{
\item{x1}{The larger variable.}

\item{x2}{The smaller variable.}

\item{y}{The third variable}

\item{name1}{The name for \code{x1} that
will be used in error messages.}

\item{name2}{The name for \code{x2} that
will be used in error messages.}

\item{name_y}{The name for \code{y} that will
be used in error messages.}
}
\description{
Check that differences between two variables
is divisible by a third variable
}
\examples{
chk_difference_divisible(x1 = 50,
                         x2 = 15,
                         y = 5,
                         name1 = "x1",
                         name2 = "x2",
                         name_y = "y")
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chk-composite.R, R/err-composite.R
\name{chk_dim_min_length}
\alias{chk_dim_min_length}
\alias{err_dim_min_length}
\title{Check that a dimension equals or exceeeds a minimum length}
\usage{
chk_dim_min_length(length_actual, length_min, name)

err_dim_min_length(length_actual, length_min, name)
}
\arguments{
\item{length_actual}{The actual length of the dimension.}

\item{length_min}{The minimum length of the dimension.}

\item{name}{The name of the dimension}
}
\description{
Check that a dimension equals or exceeeds a minimum length
}
\examples{
chk_dim_min_length(length_actual = 5L,
                   length_min = 2L,
                   name = "age")
}

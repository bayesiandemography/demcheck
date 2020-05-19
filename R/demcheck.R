
#' demcheck: Checking functions for the dem packages
#'
#' Functions for checking inputs and data structures
#' in the \code{dem} packages, eg \pkg{demarray}.
#' End users would not normally call these functions directly.
#'
#' Reduce boilerplate code, and give same -- hopefully high quality
#' -- error messages across all packages.
#'
#' Try to provide data that can help with debugging.
#'
#' Has idea of 'scalar'. Not really part of R - scalar is just
#' a vector of length 1 - but often useful, and gives
#' better error messages.
#' 
#'
#' Functions starting with \code{chk_is_} or \code{err_is_} are checking for
#' class or type, loosely defined.
#'
#' When \code{x} passes the test,
#' \code{chk} and \code{err} functions both
#' return \code{TRUE}.  When \code{x} fails the test,
#' \code{chk} functions return a string describing the failure,
#' and \code{err} functions raise an error with that string
#' as the error message.
#' 
#'
#' @docType package
#' @name demcheck
NULL



#' Checks of membership
#'
#' Functions to check whether a string belongs to a
#' set of values.  Includes a check of
#' whether \code{x} is a valid string, to provide
#' more helpful error messages to users.

#' @param x The object being checked.
#' @param name The name used in any message. Typically,
#' but not always, the name of \code{x}.
#'
#' @return When \code{x} passes the test,
#' the \code{chk*} and \code{err*} functions both
#' return \code{TRUE}.  When \code{x} fails the test,
#' the \code{chk*} functions return a string, and the \code{err*}
#' functions raise an error.
#'
#' @seealso \code{\link{single}}, \code{\link{composite}}
#' @name member
NULL

## HAS_TESTS
#' @export
#' @rdname member
chk_member_unit <- function(x, name) {
    valid_members <- c("month", "quarter", "year")
    val <- chk_is_string(x = x,
                         name = name)
    if (!isTRUE(val))
        return(val)
    i <- pmatch(x, valid_members, nomatch = 0L)
    if (i == 0L)
        return(gettextf("value for '%s' [\"%s\"] is not a permitted time unit",
                        name, x))
    TRUE
}


        

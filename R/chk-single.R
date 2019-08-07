
#' Checks of Single Conditions
#'
#' Functions to check single conditions.
#' No sanity-checking of arguments is
#' carried out. These functions are typically
#' called from  other, more complicated,
#' checking functions.
#'
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
#' @seealso \code{\link{member}}, \code{\link{composite}}
#' @name single
NULL


#' @export
#' @rdname single
chk_is_character <- function(x, name) {
    if (!is.character(x))
        return(gettextf("'%s' does not have type \"%s\"",
                        name, "character"))
    TRUE
}

#' @export
#' @rdname single
chk_is_date <- function(x, name) {
    if (!methods::is(x, "Date"))
        return(gettextf("'%s' does not have class \"%s\"",
                        name, "Date"))
    TRUE
}

#' @export
#' @rdname single
chk_is_date_equiv <- function(x, name) {
    x_date <- suppressWarnings(as.Date(x))
    is_not_equiv <- !is.na(x) & (is.na(x_date) | (x_date != x))
    if (any(is_not_equiv))
        return(gettextf("value '%s' in '%s' not equivalent to integer",
                        x[is_not_equiv][[1L]], name))
    TRUE
}

#' @export
#' @rdname single
chk_is_integer <- function(x, name) {
    if (!is.integer(x))
        return(gettextf("'%s' does not have type \"%s\"",
                        name, "integer"))
    TRUE
}

#' @export
#' @rdname single
chk_is_integer_equiv <- function(x, name) {
    x_int <- suppressWarnings(as.integer(x))
    is_not_equiv <- !is.na(x) & (is.na(x_int) | (x_int != x))
    if (any(is_not_equiv))
        return(gettextf("value '%s' in '%s' not equivalent to integer",
                        x[is_not_equiv][[1L]], name))
    TRUE
}

#' @export
#' @rdname single
chk_is_length_1 <- function(x, name) {
    if (!identical(length(x), 1L))
        return(gettextf("'%s' does not have length %d",
                        name, 1L))
    TRUE
}

#' @export
#' @rdname single
chk_is_not_na_scalar <- function(x, name) {
    if (is.na(x))
        return(gettextf("'%s' is NA",
                        name))
    TRUE
}

#' @export
#' @rdname single
chk_is_not_na_vector <- function(x, name) {
    if (any(is.na(x)))
        return(gettextf("'%s' has NAs",
                        name))
    TRUE
}

#' @export
#' @rdname single
chk_is_not_na_list <- function(x, name) {
    for (i in seq_along(x)) {
        element_i <- x[[i]]
        if (any(is.na(element_i)))
            return(gettextf("element %d of '%s' has NAs",
                            i, name))
    }                                
    TRUE
}

#' @export
#' @rdname single
chk_is_numeric <- function(x, name) {
    if (!is.numeric(x))
        return(gettextf("'%s' does not have type \"%s\"",
                        name, "numeric"))
    TRUE
}

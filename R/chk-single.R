
#' Checks of single conditions
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
chk_has_dimnames <- function(x, name) {
    if (is.null(dimnames(x)))
        return(gettextf("'%s' does not have dimnames",
                        name))
    TRUE
}

#' @export
#' @rdname single
chk_has_names_dimnames <- function(x, name) {
    if (is.null(names(dimnames(x))))
        return(gettextf("dimnames for '%s' do not have names",
                        name))
    TRUE
}

#' @export
#' @rdname single
chk_is_all_0_1 <- function(x, name) {
    if (!all(x %in% 0:1))
        return(gettextf("'%s' has values other than %d or %d'",
                        name, 0L, 1L))
    TRUE
}

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
    if (!inherits(x, "Date"))
        return(gettextf("'%s' does not have class \"%s\"",
                        name, "Date"))
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname single
chk_is_date_equiv <- function(x, name) {
    val <- tryCatch(error = function(e) e,
                    err_tdy_date(x = x,
                                 name = name))
    if (methods::is(val, "error"))
        val$message
    else
        TRUE
}

#' @export
#' @rdname single
chk_is_finite_scalar <- function(x, name) {
    if (is.infinite(x))
        return(gettextf("'%s' is infinite",
                        name))
    TRUE
}

#' @export
#' @rdname single
chk_is_finite_vector <- function(x, name) {
    if (any(is.infinite(x)))
        return(gettextf("'%s' has infinite values",
                        name))
    TRUE
}

## HAS_TESTS
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
chk_is_integer_equiv_scalar <- function(x, name) {
    val <- tryCatch(error = function(e) e,
                    err_tdy_integer_scalar(x = x,
                                           name = name))
    if (methods::is(val, "error"))
        val$message
    else
        TRUE
}

## HAS_TESTS
#' @export
#' @rdname single
chk_is_integer_equiv_vector <- function(x, name) {
    val <- tryCatch(error = function(e) e,
                    err_tdy_integer_vector(x = x,
                                           name = name))
    if (methods::is(val, "error"))
        val$message
    else
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
chk_is_logical <- function(x, name) {
    if (!is.logical(x))
        return(gettextf("'%s' does not have type \"%s\"",
                        name, "logical"))
    TRUE
}

#' @export
#' @rdname single
chk_is_not_na_dataframe <- function(x, name) {
    for (i in seq_along(x)) {
        element_i <- x[[i]]
        if (any(is.na(element_i)))
            return(gettextf("column %d of '%s' has NAs",
                            i, name))
    }                                
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
chk_is_numeric <- function(x, name) {
    if (!is.numeric(x))
        return(gettextf("'%s' does not have type \"%s\"",
                        name, "numeric"))
    TRUE
}

#' @export
#' @rdname single
chk_is_positive_length <- function(x, name) {
    if (identical(length(x), 0L))
        return(gettextf("'%s' has length %d",
                        name, 0L))
    TRUE
}





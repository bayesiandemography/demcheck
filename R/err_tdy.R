
#' Try to Tidy a Value
#'
#' Functions that try to put a value into a particular
#' format, and throw an error if this cannot be done.
#' These function do not have an chk* version, since
#' the results would be ambiguous in cases where the
#' return value was a string. Also, argument-tidying
#' is almost always done as part of the user interface,
#' when it is appropriate to raise an error if a value
#' departs from expectations.
#'
#' In function \code{err_tdy_unit}, \code{x}
#' can be one of \code{NULL}, \code{"year"},
#' \code{"quarter"}, \code{"month"},
#' \code{"years"}, \code{"quarters"}, or \code{"months"}.
#' It can also have the form \code{"n year"}, or \code{"n years"},
#' where \code{n} is a positive integer.
#' \code{NULL} is equivalent to \code{"year"}.
#' 
#' @param x The object being tidied.
#' @param x1 The first object being tidied.
#' @param x2 The second object being tidied.
#' @param x3 The third object being tidied.
#' @param name The name used in the error message. Typically,
#' but not always, the name of \code{x}.
#' @param name1 The name of the first object.
#' @param name2 The name of the second object.
#' @param name3 The name of the third object.
#' 
#' @return When err_tdy* can format \code{x} as required,
#' it returns the value; otherwise it raises an error.
#'
#' @name err_tdy
NULL

## HAS_TESTS
#' @export
#' @rdname err_tdy
err_tdy_breaks <- function(x, name) {
    err_is_positive_length(x = x,
                           name = name)
    err_is_not_na_vector(x = x,
                         name = name)
    err_is_finite_vector(x = x,
                         name = name)
    x <- err_tdy_integer_vector(x = x,
                         name = name)
    err_is_strictly_increasing(x = x,
                               name = name)
    x
}

## HAS_TESTS
#' @export
#' @rdname err_tdy
err_tdy_date <- function(x, name) {
    x_date <- tryCatch(error = function(cnd) cnd$message,
                       as.Date(x))
    if (is.character(x_date))
        stop(gettextf("'%s' [%s] not equivalent to dates : %s",
                        name, string_subset_vec(x), x_date))
    is_not_equiv <- !is.na(x) & (is.na(x_date) | (x_date != x))
    if (any(is_not_equiv))
        stop(gettextf("value '%s' in '%s' not equivalent to date",
                        x[is_not_equiv][[1L]], name))
    x_date
}

## HAS_TESTS
#' @export
#' @rdname single
err_tdy_integer_scalar <- function(x, name) {
    err_is_length_1(x = x,
                    name = name)
    x_int <- suppressWarnings(as.integer(x))
    is_not_equiv <- !is.na(x) && (is.na(x_int) || (x_int != x))
    if (is_not_equiv)
        stop(gettextf("'%s' [%s] not equivalent to integer",
                      name, x))
    x_int
}

## HAS_TESTS
#' @export
#' @rdname single
err_tdy_integer_vector <- function(x, name) {
    x_int <- suppressWarnings(as.integer(x))
    is_not_equiv <- !is.na(x) & (is.na(x_int) | (x_int != x))
    if (any(is_not_equiv))
        stop(gettextf("value '%s' in '%s' not equivalent to integer",
                      x[is_not_equiv][[1L]], name))
    x_int
}

## HAS_TESTS
#' @export
#' @rdname err_tdy
err_tdy_min_max <- function(x1, x2, name1, name2) {
    x1 <- err_tdy_integer_scalar(x = x1,
                                 name = name1)
    x2 <- err_tdy_integer_scalar(x = x2,
                                 name = name2)
    if (x2 < x1)
        stop(gettextf("'%s' [%d] is less than '%s' [%d]",
                      name2, x2, name1, x1))
    ans <- list(x1, x2)
    names(ans) <- c(name1, name2)
    ans
}

#' @export
#' @rdname err_tdy
err_tdy_unit <- function(x, name) {
    valid_values <- c("year", "quarter", "month")
    if (is.null(x))
        return("year")
    val <- err_is_string(x = x,
                         name = name)
    ## check for unit with no number
    for (value in valid_values) {
        pattern <- sprintf("^%ss?$", value)
        found <- grepl(pattern, x)
        if (found)
            return(x)
    }
    ## check for number plus year
    is_num_year <- grepl("^-?[0-9]+[ ]+years?$", x)
    if (is_num_year) {
        n <- as.integer(sub("^(-?[0-9]+).*$", "\\1", x))
        if (n < 1L)
            stop(gettextf("'%s' has invalid value [\"%s\"] : number of years less than %d",
                          name, x, 1L))
        return(x)
    }
    stop(gettextf("'%s' has invalid value [\"%s\"]",
                  name, x))
}

## HAS_TESTS
#' @export
#' @rdname err_tdy
err_tdy_width <- function(x1, x2, x3, name1, name2, name3) {
    x1 <- err_tdy_integer_scalar(x = x1,
                                 name = name1)
    l <- err_tdy_min_max(x1 = x2,
                         x2 = x3,
                         name1 = name2,
                         name2 = name3)
    x2 <- l[[name2]]
    x3 <- l[[name3]]
    diff <- x3 - x2
    if (diff == 0L) {
        if (x1 != 0L)
            stop(gettextf("'%s' equals '%s' but '%s' does not equal %d",
                          name2, name3, name1, 0L))
    }
    else {
        if (diff %% x1 != 0L)
            stop(gettextf("'%s' [%d] does not divide evenly into the difference between '%s' [%d] and '%s' [%d]",
                          name1, x1, name3, x3, name2, x2))
    }
    x1
}


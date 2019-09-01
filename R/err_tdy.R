
#' Try to tidy a value
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
#' @inheritParams composite
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
    if (inherits(x, "Date"))
        return(x)
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
#' @rdname err_tdy
err_tdy_date_dob <- function(date, dob) {
    date <- err_tdy_date(x = date,
                         name = "date")
    dob <- err_tdy_date(x = dob,
                        name = "dob")
    l <- err_tdy_same_length(x1 = date,
                             x2 = dob,
                             name1 = "date",
                             name2 = "dob")
    date <- l$date
    dob <- l$dob
    err_is_ge_vector(x1 = date,
                     x2 = dob,
                     name1 = "date",
                     name2 = "dob")
    date <- as.POSIXlt(date)
    dob <- as.POSIXlt(dob)
    list(date = date,
         dob = dob)
}

## HAS_TESTS
#' @export
#' @rdname err_tdy
err_tdy_first_month <- function(x, name) {
    err_is_string(x = x,
                  name = name)
    s <- ISOdate(2000, 1:12, 1)
    valid_abb <- format(s, format = "%b")
    valid_full <- format(s, format = "%B")
    i <- match(tolower(x), tolower(valid_abb), nomatch = 0L)
    if (i > 0L)
        return(valid_abb[[i]])
    i <- match(tolower(x), tolower(valid_full), nomatch = 0L)
    if (i > 0L)
        return(valid_abb[[i]])
    stop(gettextf("invalid value for '%s' : \"%s\"",
                  name, x))
}


## HAS_TESTS
#' @export
#' @rdname err_tdy
err_tdy_integer_scalar <- function(x, name) {
    err_is_length_1(x = x,
                    name = name)
    if (is.integer(x))
        return(x)
    x_int <- suppressWarnings(as.integer(x))
    is_not_equiv <- !is.na(x) && (is.na(x_int) || (x_int != x))
    if (is_not_equiv)
        stop(gettextf("'%s' [%s] not equivalent to integer",
                      name, x))
    x_int
}

## HAS_TESTS
#' @export
#' @rdname err_tdy
err_tdy_integer_vector <- function(x, name) {
    if (is.integer(x))
        return(x)
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
err_tdy_many_to_one <- function(x, name) {
    if (!is.data.frame(x))
        stop(gettextf("'%s' is not a data.frame",
                      name))
    if (!identical(length(x), 2L))
        stop(gettextf("'%s' does not have %d columns",
                      name, 2L))
    if (identical(nrow(x), 0L))
        stop(gettextf("'%s' has %d rows",
                      name, 0L))
    err_is_not_na_dataframe(x = x,
                            name = name)
    is_unique <- sapply(x, function(x) !any(duplicated(x)))
    if (all(is_unique))
        stop(gettextf("neither column of '%s' has duplicates, as required for many-to-one mapping",
                      name))
    if (!any(is_unique))
        stop(gettextf("neither column of '%s' has entirely unique values, as required for many-to-one mapping",
                      name))
    x[] <- lapply(x, as.character)
    x        
}

## HAS_TESTS
#' @export
#' @rdname err_tdy
err_tdy_positive_integer_scalar <- function(x, name) {
    err_is_positive_scalar(x = x,
                           name = name)
    if (!is.integer(x)) {
        x_int <- suppressWarnings(as.integer(x))
        is_not_equiv <- is.na(x_int) || (x_int != x)
        if (is_not_equiv)
            stop(gettextf("'%s' [%s] not equivalent to integer",
                          name, x))
    }
    x_int
}

## HAS_TESTS
#' @export
#' @rdname err_tdy
err_tdy_same_length <- function(x1, x2, name1, name2) {
    err_length_same_or_1(x1 = x1,
                         x2 = x2,
                         name1 = name1,
                         name2 = name2)
    n1 <- length(x1)
    n2 <- length(x2)
    if (n1 > n2)
        x2 <- rep(x2, times = n1)
    if (n2 > n1)
        x1 <- rep(x1, times = n2)
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


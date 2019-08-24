
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
#' @param width The width of age or time intervals.
#' @param first_day A string specifying the first
#' day of the year, for year-long periods, eg
#' "1 January", "Jul-1".
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
err_tdy_first_day <- function(first_day) {
    fmts <- c("%d %b %Y",
              "%d %B %Y",
              "%d-%b %Y",
              "%d-%B %Y",
              "%b %d %Y",
              "%B %d %Y",
              "%b-%d %Y",
              "%B-%d %Y")
    err_is_string(x = first_day,
                  name = "first_day")
    date <- tryCatch(error = function(e) e,
                     as.Date(paste(first_day, "2001"),
                             tryFormats = fmts))
    if (inherits(date, "error"))
        stop(gettextf("invalid value for '%s' : \"%s\"",
                      "first_day", first_day))
    date <- as.POSIXlt(date)
    mday <- date$mday
    mon <- date$mon
    list(mday = mday,
         mon = mon)
}


## HAS_TESTS
#' @export
#' @rdname single
err_tdy_integer_scalar <- function(x, name) {
    if (is.integer(x))
        return(x)
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
err_tdy_min_max <- function(min, max) {
    min <- err_tdy_integer_scalar(x = min,
                                 name = "min")
    max <- err_tdy_integer_scalar(x = max,
                                 name = "max")
    if (max <= min)
        stop(gettextf("'%s' [%d] is less than or equal to '%s' [%d]",
                      "max", max, "min", min))
    list(min = min, max = max)
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

## HAS_TESTS
#' @export
#' @rdname err_tdy
err_tdy_width <- function(width, min, max) {
    width <- err_tdy_integer_scalar(x = width,
                                    name = "width")
    l <- err_tdy_min_max(min = min,
                         max = max)
    min <- l$min
    max <- l$max
    diff <- max - min
    if (diff %% width != 0L)
        stop(gettextf("'%s' [%d] does not divide evenly into the difference between '%s' [%d] and '%s' [%d]",
                      "width", width, "max", max, "min", min))
    width
}

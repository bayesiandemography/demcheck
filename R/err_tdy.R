
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
#' @param breaks Dates or integers.
#' @param equal_ok Whether 'break_min' and 'break_max'
#' can be equal.
#' @param open_first Logical. Whether interval open on left.
#' @param open_last Logical. Whether interval open on right.
#' 
#' @return When err_tdy* can format \code{x} as required,
#' it returns the value; otherwise it raises an error.
#'
#' @name err_tdy
NULL

## HAS_TESTS
#' @export
#' @rdname err_tdy
err_tdy_break_min_max_date <- function(break_min, break_max, unit, null_ok, equal_ok) {
    min_null <- is.null(break_min)
    max_null <- is.null(break_max)
    if (null_ok) {
        if (min_null && max_null)
            stop(gettextf("'%s' and '%s' both %s",
                          "break_min", "break_max", "NULL"),
                 call. = FALSE)
    }
    else {
        if (min_null)
            stop(gettextf("'%s' is %s",
                          "break_min", "NULL"),
                 call. = FALSE)
        if (max_null)
            stop(gettextf("'%s' is %s",
                          "break_max", "NULL"),
                 call. = FALSE)
    }
    if (!min_null) {
        break_min <- err_tdy_date_scalar(x = break_min,
                                         name = "break_min")
        err_is_first_day_unit_scalar(x = break_min,
                                     name = "break_min",
                                     unit = unit)
    }
    if (!max_null) {
        break_max <- err_tdy_date_scalar(x = break_max,
                                         name = "break_max")
        err_is_first_day_unit_scalar(x = break_max,
                                     name = "break_max",
                                     unit = unit)
    }
    if (!min_null && !max_null) {
        if (equal_ok)
            err_is_ge_scalar(x1 = break_max,
                             x2 = break_min,
                             name1 = "break_max",
                             name2 = "break_min")
        else
            err_is_gt_scalar(x1 = break_max,
                             x2 = break_min,
                             name1 = "break_max",
                             name2 = "break_min")
    }
    list(break_min = break_min,
         break_max = break_max)
}

## HAS_TESTS
#' @export
#' @rdname err_tdy
err_tdy_break_min_max_integer <- function(break_min, break_max, null_ok, equal_ok) {
    min_null <- is.null(break_min)
    max_null <- is.null(break_max)
    if (null_ok) {
        if (min_null && max_null)
            stop(gettextf("'%s' and '%s' both %s",
                          "break_min", "break_max", "NULL"),
                 call. = FALSE)
    }
    else {
        if (min_null)
            stop(gettextf("'%s' is %s",
                          "break_min", "NULL"),
                 call. = FALSE)
        if (max_null)
            stop(gettextf("'%s' is %s",
                          "break_max", "NULL"),
                 call. = FALSE)
    }
    break_min <- err_tdy_non_negative_integer_scalar(x = break_min, 
                                                     name = "break_min",
                                                     null_ok = null_ok)
    if (equal_ok)
        break_max <- err_tdy_non_negative_integer_scalar(x = break_max,
                                                        name = "break_max",
                                                        null_ok = null_ok)
    else    
        break_max <- err_tdy_positive_integer_scalar(x = break_max,
                                                     name = "break_max",
                                                     null_ok = null_ok)
    if (!min_null && !max_null) {
        if (equal_ok)
            err_is_ge_scalar(x1 = break_max,
                             x2 = break_min,
                             name1 = "break_max",
                             name2 = "break_min")
        else
            err_is_gt_scalar(x1 = break_max,
                             x2 = break_min,
                             name1 = "break_max",
                             name2 = "break_min")            
    }
    list(break_min = break_min,
         break_max = break_max)
}

## HAS_TESTS
#' @export
#' @rdname err_tdy
err_tdy_breaks_date_cohort <- function(breaks, open_first) {
    n <- length(breaks)
    if (n == 0L) {
        if (open_first)
            stop(gettextf("'%s' has length %d but '%s' is %s",
                          "breaks", 0L, "open_first", "TRUE"),
                 call. = FALSE)
        return(as.Date(as.character()))
    }
    if (n == 1L) {
        if (!open_first)
            stop(gettextf("'%s' has length %d but '%s' is %s",
                          "breaks", 1L, "open_first", "FALSE"),
                 call. = FALSE)
    }
    err_is_not_na_vector(x = breaks,
                         name = "breaks")
    breaks <- err_tdy_date_vector(x = breaks,
                                  name = "breaks")
    err_is_strictly_increasing(x = breaks,
                               name = "breaks")
    breaks
}

## HAS_TESTS
#' @export
#' @rdname err_tdy
err_tdy_breaks_date_period <- function(breaks) {
    n <- length(breaks)
    if (n == 1L)
        stop(gettextf("'%s' has length %d",
                      "breaks", 1L),
             call. = FALSE)
    err_is_not_na_vector(x = breaks,
                         name = "breaks")
    breaks <- err_tdy_date_vector(x = breaks,
                                  name = "breaks")
    err_is_strictly_increasing(x = breaks,
                               name = "breaks")
    breaks
}

## HAS_TESTS
#' @export
#' @rdname err_tdy
err_tdy_breaks_integer_age <- function(breaks, open_last) {
    n <- length(breaks)
    if (n == 0L) {
        if (open_last)
            stop(gettextf("'%s' has length %d but '%s' is %s",
                          "breaks", 0L, "open_last", "TRUE"))
    }
    if (n == 1L) {
        if (!open_last)
            stop(gettextf("'%s' has length %d but '%s' is %s",
                          "breaks", 1L, "open_last", "FALSE"))
    }
    err_is_not_na_vector(x = breaks,
                         name = "breaks")
    err_is_finite_vector(x = breaks,
                         name = "breaks")
    err_is_non_negative_vector(x = breaks,
                               name = "breaks")
    err_is_integer_equiv_vector(x = breaks,
                                name = "breaks")
    err_is_strictly_increasing(x = breaks,
                               name = "breaks")
    as.integer(breaks)
}

## HAS_TESTS
#' @export
#' @rdname err_tdy
err_tdy_breaks_integer_enum <- function(breaks, open_first, open_last) {
    n <- length(breaks)
    if (n == 0L) {
        if (open_first)
            stop(gettextf("'%s' has length %d but '%s' is %s",
                          "breaks", 0L, "open_first", "TRUE"))
        if (open_last)
            stop(gettextf("'%s' has length %d but '%s' is %s",
                          "breaks", 0L, "open_last", "TRUE"))
    }
    if (n == 1L) {
        if (!open_first && !open_last)
            stop(gettextf("'%s' has length %d but '%s' and '%s' are both %s",
                          "breaks", 1L, "open_first", "open_last", "FALSE"))
    }
    err_is_not_na_vector(x = breaks,
                         name = "breaks")
    err_is_finite_vector(x = breaks,
                         name = "breaks")
    err_is_integer_equiv_vector(x = breaks,
                                name = "breaks")
    err_is_strictly_increasing(x = breaks,
                               name = "breaks")
    as.integer(breaks)
}

## HAS_TESTS
#' @export
#' @rdname err_tdy
err_tdy_date_scalar <- function(x, name) {
    err_is_length_1(x = x,
                    name = name)
    if (inherits(x, "Date"))
        return(x)
    x_date <- tryCatch(error = function(X) X,
                       as.Date(x))
    if (inherits(x_date, "error"))
        stop(gettextf("'%s' [\"%s\"] not equivalent to date : %s",
                      name, x, x_date$message),
             call. = FALSE)
    x_date
}

## HAS_TESTS
#' @export
#' @rdname err_tdy
err_tdy_date_vector <- function(x, name) {
    if (inherits(x, "Date"))
        return(x)
    x_date <- tryCatch(error = function(X) X,
                       as.Date(x))
    if (inherits(x_date, "error"))
        stop(gettextf("'%s' [%s] not equivalent to dates : %s",
                      name, string_subset_vec(x), x_date$message),
             call. = FALSE)
    is_not_equiv <- !is.na(x) & (is.na(x_date) | (x_date != x))
    i_not_equiv <- match(TRUE, is_not_equiv, nomatch = 0L)
    if (i_not_equiv > 0L)
        stop(gettextf("value \"%s\" in '%s' not equivalent to date",
                      x[[i_not_equiv]], name),
             call. = FALSE)
    x_date
}

## HAS_TESTS
#' @export
#' @rdname err_tdy
err_tdy_date_dob <- function(date, dob) {
    date <- err_tdy_date_vector(x = date,
                                name = "date")
    dob <- err_tdy_date_vector(x = dob,
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
    list(date = date,
         dob = dob)
}

## HAS_TESTS
#' @export
#' @rdname err_tdy
err_tdy_integer_scalar <- function(x, name, null_ok = FALSE) {
    if (is.null(x)) {
        if (null_ok)
            return(x)
        else
            stop(gettextf("'%s' is %s",
                          name, "NULL"),
                 call. = FALSE)
    }
    err_is_length_1(x = x,
                    name = name)
    if (is.integer(x))
        return(x)
    x_int <- suppressWarnings(as.integer(x))
    is_not_equiv <- !is.na(x) && (is.na(x_int) || (x_int != x))
    if (is_not_equiv)
        stop(gettextf("'%s' [%s] not equivalent to integer",
                      name, x),
             call. = FALSE)
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
                      x[is_not_equiv][[1L]], name),
             call. = FALSE)
    x_int
}

## HAS_TESTS
#' @export
#' @rdname err_tdy
err_tdy_many_to_one <- function(x, name) {
    if (!is.data.frame(x))
        stop(gettextf("'%s' is not a data.frame",
                      name),
             call. = FALSE)
    if (!identical(length(x), 2L))
        stop(gettextf("'%s' does not have %d columns",
                      name, 2L),
             call. = FALSE)
    if (identical(nrow(x), 0L))
        stop(gettextf("'%s' has %d rows",
                      name, 0L),
             call. = FALSE)
    err_is_not_na_dataframe(x = x,
                            name = name)
    is_unique <- sapply(x, function(x) !any(duplicated(x)))
    if (all(is_unique))
        stop(gettextf("neither column of '%s' has duplicates, as required for many-to-one mapping",
                      name),
             call. = FALSE)
    if (!any(is_unique))
        stop(gettextf("neither column of '%s' has entirely unique values, as required for many-to-one mapping",
                      name))
    x[] <- lapply(x, as.character)
    x        
}

## HAS_TESTS
#' @export
#' @rdname err_tdy
err_tdy_month_start <- function(x, name) {
    err_is_string(x = x,
                  name = name)
    valid_abb <- month.abb
    valid_full <- month.name
    i <- match(tolower(x), tolower(valid_abb), nomatch = 0L)
    if (i > 0L)
        return(valid_abb[[i]])
    i <- match(tolower(x), tolower(valid_full), nomatch = 0L)
    if (i > 0L)
        return(valid_abb[[i]])
    stop(gettextf("invalid value for '%s' : \"%s\"",
                  name, x),
         call. = FALSE)
}

## HAS_TESTS
#' @export
#' @rdname err_tdy
err_tdy_non_negative_integer_scalar <- function(x, name, null_ok = FALSE) {
    if (is.null(x)) {
        if (null_ok)
            return(x)
        else
            stop(gettextf("'%s' is %s",
                          name, "NULL"),
                 call. = FALSE)
    }
    err_is_non_negative_scalar(x = x,
                               name = name)
    if (is.integer(x))
        return(x)
    x_int <- suppressWarnings(as.integer(x))
    is_not_equiv <- is.na(x_int) || (x_int != x)
    if (is_not_equiv)
        stop(gettextf("'%s' [%s] not equivalent to integer",
                      name, x),
             call. = FALSE)
    x_int
}

## HAS_TESTS
#' @export
#' @rdname err_tdy
err_tdy_positive_integer_scalar <- function(x, name, null_ok = FALSE) {
    if (is.null(x)) {
        if (null_ok)
            return(x)
        else
            stop(gettextf("'%s' is %s",
                          name, "NULL"),
                 call. = FALSE)
    }
    err_is_positive_scalar(x = x,
                           name = name)
    if (is.integer(x))
        return(x)
    x_int <- suppressWarnings(as.integer(x))
    is_not_equiv <- is.na(x_int) || (x_int != x)
    if (is_not_equiv)
        stop(gettextf("'%s' [%s] not equivalent to integer",
                      name, x),
             call. = FALSE)
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
                          name, x, 1L),
                 call. = FALSE)
        return(x)
    }
    stop(gettextf("'%s' has invalid value [\"%s\"]",
                  name, x))
}



#' Checks of composite conditions
#'
#' Functions to check for conditions that involve
#' several attributes. For instance, \code{chk_is_integer_consec}
#' checks whether \code{x} has type integer, whether \code{x}
#' has no \code{NA}s, and whether \code{all(diff(x)) == 1L}.
#'
#' \code{chk_is_first_day_unit} checks whether a \code{x}
#' consists of first days for the time unit supplied,
#' eg values such \code{"2001-03-01"}, \code{"2001-05-01"}, or
#' \code{"2000-09-01"} when \code{unit} is \code{"months"}.
#' \code{chk_is_first_day_unit_consec} adds the condition
#' that these dates be consecutive, eg
#' \code{"2001-03-01"}, \code{"2001-04-01"}, \code{"2000-09-01"}.
#' Separately checking for first days makes the job of
#' \code{chk_is_first_day_unit_consec} much easier, which
#' is why we have two functions.
#'
#' Most of the functions raise an error if \code{x} has \code{NA}s.
#' 
#' @param x The object being checked.
#' @param x1 The first of a pair of objects being checked.
#' @param x2 The second of a pair of objects being checked.
#' @param name The name used in any message. Typically,
#' but not always, the name of \code{x}.
#' @param name1 The name of the first of the pair of objects.
#' @param name2 The name of the second of the pair of objects.
#' @param unit Measurement units for time, eg \code{"month"}.
#' @param age Age, typically in years, but can be other unit.
#' @param min Minimum age or time.
#' @param max Maximum age or time.
#' @param date Date on which event occurred or measurement made.
#' Object of class "Date".
#' @param dob Date of birth. Object of class "Date".
#'
#' @return When \code{x} passes the test,
#' the \code{chk*} and \code{err*} functions both
#' return \code{TRUE}.  When \code{x} fails the test,
#' the \code{chk*} functions return a string, and the \code{err*}
#' functions raise an error.
#'
#' @seealso \code{\link{single}}, \code{\link{member}}
#' @name composite
NULL

## HAS_TESTS
#' @export
#' @rdname composite
chk_age_ge_min <- function(age, min, date, dob, unit) {
    less_than_min <- !is.na(age) & (age < min)
    if (any(less_than_min)) {
        i <- match(TRUE, less_than_min)
        return(gettextf("'%s' [\"%s\"] and '%s' [\"%s\"] imply an age of %d %ss, which is less than '%s' [%d %ss]",
                        "date", date[[i]], "dob", dob[[i]], age[[i]], unit, "min", min, unit))
    }
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname composite
chk_age_lt_max <- function(age, max, date, dob, unit) {
    ge_max <- !is.na(age) & (age >= max)
    if (any(ge_max)) {
        i <- match(TRUE, ge_max)
        return(gettextf("'%s' [\"%s\"] and '%s' [\"%s\"] imply an age of %d %ss, which is greater than or equal to '%s' [%d %ss]",
                        "date", date[[i]], "dob", dob[[i]], age[[i]], unit, "max", max, unit))
    }
    TRUE
}

## HAS_TESTS
## array has metadata required to uniquely identify
## every cell, using only dimnames and names of
## dimnames
#' @export
#' @rdname composite
chk_array_metadata_complete <- function(x, name) {
    val <- chk_has_dimnames(x = x,
                            name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_has_names_dimnames(x = x,
                                  name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_names_dimnames_complete(x = x,
                                       name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_dimnames_complete(x = x,
                                 name = name)
    if (!isTRUE(val))
        return(val)
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname composite
chk_is_first_day_unit <- function(x, name, unit) {
    n <- length(x)
    if (n == 0L)
        return(TRUE)
    val <- chk_is_not_na_vector(x = x, name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_is_date_equiv(x = x, name = name)
    if (!isTRUE(val))
        return(val)
    x <- as.Date(x)
    val <- chk_member_unit(x = unit, name = "unit")
    if (!isTRUE(val))
        return(val)
    year <- as.integer(format(x, "%Y"))
    from <- as.Date(sprintf("%d-01-01", min(year)))
    to <- as.Date(sprintf("%d-01-01", max(year) + 1L))
    seq_expected <- seq.Date(from = from,
                             to = to,
                             by = unit)
    is_not_in_seq <- !(x %in% seq_expected)
    if (any(is_not_in_seq)) {
        i <- match(TRUE, is_not_in_seq)
        return(gettextf("element %d [\"%s\"] of '%s' is not the first day of the %s",
                        i,
                        format(x[[i]], "%Y-%m-%d"),
                        name,
                        unit))
    }
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname composite
chk_is_first_day_unit_consec <- function(x, name, unit) {
    n <- length(x)
    if (n == 0L)
        return(TRUE)
    val <- chk_is_first_day_unit(x = x,
                                      name = name,
                                      unit = unit)
    if (!isTRUE(val))
        return(val)
    if (n >= 2L) {
        from <- x[[1L]]
        seq_expected <- seq.Date(from = from,     # Calculation using 'seq.Date' relies
                                 by = unit,  # on each date being the first day
                                 length.out = n)  # of the month
        is_not_equal_to_seq <- x != seq_expected
        if (any(is_not_equal_to_seq)) {
            i <- match(TRUE, is_not_equal_to_seq)
            return(gettextf("dates \"%s\" and \"%s\" in '%s' do not belong to consecutive %ss",
                            format(x[[i - 1L]], "%Y-%m-%d"),
                            format(x[[i]], "%Y-%m-%d"),
                            name,
                            unit))
        }
    }
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname composite
chk_is_ge_scalar <- function(x1, x2, name1, name2) {
    if (!is.na(x1) && !is.na(x2) && (x1 < x2))
        return(gettextf("'%s' [%s] is less than '%s' [%s]",
                        name1, x1, name2, x2))
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname composite
chk_is_ge_vector <- function(x1, x2, name1, name2) {
    is_lt <- !is.na(x1) & !is.na(x2) & (x1 < x2)
    if (any(is_lt)) {
        i <- match(TRUE, is_lt)
        return(gettextf("element %d of '%s' [%s] is less than element %d of '%s' [%s]",
                        i, name1, x1[[i]], i, name2, x2[[i]]))
    }
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname composite
chk_is_integer_consec <- function(x, name) {
    val <- chk_is_integer(x = x, name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_is_not_na_vector(x = x, name = name)
    if (!isTRUE(val))
        return(val)
    if (length(x) >= 2L) {
        diff <- diff(x)
        is_not_one <- diff != 1L
        if (any(is_not_one)) {
            i <- match(TRUE, is_not_one)
            return(gettextf("elements %d [%d] and %d [%d] of '%s' are not consecutive integers",
                            i, x[[i]], i + 1L, x[[i + 1L]], name))
        }
    }
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname composite
chk_is_logical_flag <- function(x, name) {
    val <- chk_is_logical(x = x, name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_is_length_1(x = x, name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_is_not_na_scalar(x = x, name = name)
    if (!isTRUE(val))
        return(val)
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname single
chk_is_non_negative_scalar <- function(x, name) {
    val <- chk_is_length_1(x = x,
                           name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_is_not_na_scalar(x = x,
                                name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_is_numeric(x = x,
                          name = name)
    if (!isTRUE(val))
        return(val)
    if (x < 0L)
        return(gettextf("'%s' [%s] is negative",
                        name, x))
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname single
chk_is_non_negative_vector <- function(x, name) {
    val <- chk_is_not_na_vector(x = x,
                                name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_is_numeric(x = x,
                          name = name)
    if (!isTRUE(val))
        return(val)
    is_neg <- x < 0L
    if (any(is_neg)) {
        i <- match(TRUE, is_neg)
        return(gettextf("element %d of '%s' [%s] is negative",
                        i, name, x[[i]]))
    }
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname single
chk_is_positive_scalar <- function(x, name) {
    val <- chk_is_length_1(x = x,
                           name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_is_not_na_scalar(x = x,
                                name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_is_numeric(x = x,
                          name = name)
    if (!isTRUE(val))
        return(val)
    if (x <= 0L)
        return(gettextf("'%s' [%s] is non-positive",
                        name, x))
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname single
chk_is_positive_vector <- function(x, name) {
    val <- chk_is_not_na_vector(x = x,
                                name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_is_numeric(x = x,
                          name = name)
    if (!isTRUE(val))
        return(val)
    is_non_pos <- x <= 0L
    if (any(is_non_pos)) {
        i <- match(TRUE, is_non_pos)
        return(gettextf("element %d of '%s' [%s] is non-positive",
                        i, name, x[[i]]))
    }
    TRUE
}


## HAS_TESTS
#' @export
#' @rdname composite
chk_is_string <- function(x, name) {
    val <- chk_is_character(x = x, name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_is_length_1(x = x, name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_is_not_na_scalar(x = x, name = name)
    if (!isTRUE(val))
        return(val)
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname composite
chk_is_strictly_increasing <- function(x, name) {
    val <- chk_is_numeric(x = x, name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_is_not_na_vector(x = x, name = name)
    if (!isTRUE(val))
        return(val)
    if (length(x) >= 2L) {
        is_not_incr <- diff(x) <= 0L
        if (any(is_not_incr)) {
            i <- match(TRUE, is_not_incr)
            return(gettextf("'%s' is not strictly increasing : element %d [%s] is greater than or equal to element %d [%s]",
                            name, i, x[[i]], i + 1L, x[[i + 1L]]))
        }
    }
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname composite
chk_length_same_or_1 <- function(x1, x2, name1, name2) {
    n1 <- length(x1)
    n2 <- length(x2)
    if (n1 == 0L)
        return(gettextf("'%s' has length %d",
                        name1, 0L))
    if (n2 == 0L)
        return(gettextf("'%s' has length %d",
                        name2, 0L))
    if (n1 == n2)
        return(TRUE)
    if ((n1 == 1L) || (n2 == 1L))
        return(TRUE)
    gettextf("'%s' has length %d and '%s' has length %d : should have same lengths, or one should have length %d",
             name1, n1, name2, n2, 1L)
}

## HAS_TESTS
#' @export
#' @rdname composite
chk_dimnames_complete <- function(x, name) {
    dimnames <- dimnames(x)
    names <- names(dimnames)
    dim <- dim(x)
    for (i in seq_along(dimnames)) {
        if (dim[[i]] > 0L) {
            dimnames_i <- dimnames[[i]]
            names_i <- names[[i]]
            if (is.null(dimnames_i))
                return(gettextf("\"%s\" dimension of '%s' does not have dimnames",
                                names_i, name))
            if (any(is.na(dimnames_i)))
                return(gettextf("dimnames for \"%s\" dimension of '%s' have NAs",
                                names_i, name))
            if (!all(nzchar(dimnames_i)))
                return(gettextf("dimnames for \"%s\" dimension of '%s' have blanks",
                                names_i, name))
            is_duplicated <- duplicated(dimnames_i)
            if (any(is_duplicated)) {
                j <- match(TRUE, is_duplicated)
                return(gettextf("dimnames for \"%s\" dimension of '%s' have duplicate [\"%s\"]",
                                names_i, name, dimnames_i[[j]]))
            }
        }
    }
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname composite
chk_names_complete <- function(x, name) {
    nms <- names(x)
    if (any(is.na(nms)))
        return(gettextf("names for '%s' have NAs",
                        name))
    if (!all(nzchar(nms)))
        return(gettextf("names for '%s' have blanks",
                        name))
    is_duplicated <- duplicated(nms)
    if (any(is_duplicated)) {
        i <- match(TRUE, is_duplicated)
        return(gettextf("names for '%s' have duplicate [\"%s\"]",
                        name, nms[[i]]))
    }
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname composite
chk_names_dimnames_complete <- function(x, name) {
    nms <- names(dimnames(x))
    if (any(is.na(nms)))
        return(gettextf("names for dimnames of '%s' have NAs",
                        name))
    if (!all(nzchar(nms)))
        return(gettextf("names for dimnames of '%s' have blanks",
                        name))
    is_duplicated <- duplicated(nms)
    if (any(is_duplicated)) {
        i <- match(TRUE, is_duplicated)
        return(gettextf("names for dimnames of '%s' have duplicate [\"%s\"]",
                        name, nms[[i]]))
    }
    TRUE
}



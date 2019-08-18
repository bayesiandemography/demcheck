
#' Checks of Composite Conditions
#'
#' Functions to check for conditions that involve
#' several attributes. For instance, \code{chk_is_integer_consec}
#' checks whether \code{x} has type integer, whether \code{x}
#' has no \code{NA}s, and whether \code{all(diff(x)) == 1L}.
#'
#' \code{chk_is_first_day_time_unit} checks whether a \code{x}
#' consists of first days for the time unit supplied,
#' eg values such \code{"2001-03-01"}, \code{"2001-05-01"}, or
#' \code{"2000-09-01"} when \code{time_unit} is \code{"months"}.
#' \code{chk_is_first_day_time_unit_consec} adds the condition
#' that these dates be consecutive, eg
#' \code{"2001-03-01"}, \code{"2001-04-01"}, \code{"2000-09-01"}.
#' Separately checking for first days makes the job of
#' \code{chk_is_first_day_time_unit_consec} much easier, which
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
#' @param time_unit Measurement units for time, eg \code{"month"}.
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
chk_is_first_day_time_unit <- function(x, name, time_unit) {
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
    val <- chk_member_time_unit(x = time_unit, name = "time_unit")
    if (!isTRUE(val))
        return(val)
    year <- as.integer(format(x, "%Y"))
    from <- as.Date(sprintf("%d-01-01", min(year)))
    to <- as.Date(sprintf("%d-01-01", max(year) + 1L))
    seq_expected <- seq.Date(from = from,
                             to = to,
                             by = time_unit)
    is_not_in_seq <- !(x %in% seq_expected)
    if (any(is_not_in_seq)) {
        i <- match(TRUE, is_not_in_seq)
        return(gettextf("element %d [\"%s\"] of '%s' is not the first day of the %s",
                        i,
                        format(x[[i]], "%Y-%m-%d"),
                        name,
                        time_unit))
    }
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname composite
chk_is_first_day_time_unit_consec <- function(x, name, time_unit) {
    n <- length(x)
    if (n == 0L)
        return(TRUE)
    val <- chk_is_first_day_time_unit(x = x,
                                      name = name,
                                      time_unit = time_unit)
    if (!isTRUE(val))
        return(val)
    if (n >= 2L) {
        from <- x[[1L]]
        seq_expected <- seq.Date(from = from,     # Calculation using 'seq.Date' relies
                                 by = time_unit,  # on each date being the first day
                                 length.out = n)  # of the month
        is_not_equal_to_seq <- x != seq_expected
        if (any(is_not_equal_to_seq)) {
            i <- match(TRUE, is_not_equal_to_seq)
            return(gettextf("dates \"%s\" and \"%s\" in '%s' do not belong to consecutive %ss",
                            format(x[[i - 1L]], "%Y-%m-%d"),
                            format(x[[i]], "%Y-%m-%d"),
                            name,
                            time_unit))
        }
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



## Try to tidy a value
##
## Functions that try to put a value into a particular
## format, and throw an error if this cannot be done.
## These function do not have an chk* version, since
## the results would be ambiguous in cases where the
## return value was a string. Also, argument-tidying
## is almost always done as part of the user interface,
## when it is appropriate to raise an error if a value
## departs from expectations.


## HAS_TESTS
#' Check and tidy minimum and maximum dates
#'
#' @param break_min Date or NULL. If non-NULL, lowest permissible value
#' for \code{date}.
#' @param break_max Date or NULL. If non-NULL, highest permissible value
#' for \code{date}.
#' @param unit A time unit.
#' @param null_ok Whether \code{break_min} and \code{break_max} can be
#' \code{NULL}
#' @param equal_ok Whether \code{break_min} and \code{break_max} can be
#' \code{NULL}.
#'
#' @seealso \code{\link{err_tdy_break_min_max_integer}}
#'
#' @examples
#' err_tdy_break_min_max_date(break_min = as.Date("2000-01-01"),
#'                            break_max = as.Date("2080-01-01"),
#'                            unit = "year",
#'                            null_ok = FALSE,
#'                            equal_ok = FALSE)
#' @export
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
        err_first_day_unit_scalar(x = break_min,
                                     name = "break_min",
                                     unit = unit)
    }
    if (!max_null) {
        break_max <- err_tdy_date_scalar(x = break_max,
                                         name = "break_max")
        err_first_day_unit_scalar(x = break_max,
                                     name = "break_max",
                                     unit = unit)
    }
    if (!min_null && !max_null) {
        if (equal_ok)
            err_ge_scalar(x1 = break_max,
                             x2 = break_min,
                             name1 = "break_max",
                             name2 = "break_min")
        else
            err_gt_scalar(x1 = break_max,
                             x2 = break_min,
                             name1 = "break_max",
                             name2 = "break_min")
    }
    list(break_min = break_min,
         break_max = break_max)
}


## HAS_TESTS
#' Check and tidy minimum and maximum integers
#'
#' Typically used for age or cohort.
#'
#' @inheritParams err_tdy_break_min_max_date
#' @param break_min An integer, a number that can
#' be coercted to integer, or NULL.
#' @param break_max An integer, a number that can
#' be coercted to integer, or NULL.
#'
#' @seealso \code{\link{err_tdy_break_min_max_date}},
#' 
#' @examples
#' err_tdy_break_min_max_integer(break_min = 0L,
#'                               break_max = 100L,
#'                               null_ok = FALSE,
#'                               equal_ok = FALSE)
#' @export
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
            err_ge_scalar(x1 = break_max,
                             x2 = break_min,
                             name1 = "break_max",
                             name2 = "break_min")
        else
            err_gt_scalar(x1 = break_max,
                             x2 = break_min,
                             name1 = "break_max",
                             name2 = "break_min")            
    }
    list(break_min = break_min,
         break_max = break_max)
}


## HAS_TESTS
#' Check and tidy a vector of breaks used to define cohorts
#'
#' @param breaks A vector of dates, or values that can be coerced to dates.
#' @param open_first Whether the first interval is open on the left.
#'
#' @seealso \code{\link{err_tdy_breaks_date_period}},
#' \code{\link{err_tdy_breaks_integer_age}},
#' \code{\link{err_tdy_breaks_integer_cohort}},
#' \code{\link{err_tdy_breaks_integer_period}}
#'
#' @examples
#' breaks <- c("2000-01-01", "2010-01-01", "2020-01-01")
#' err_tdy_breaks_date_cohort(breaks = breaks,
#'                            open_first = TRUE)
#' @export
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
    err_not_na_vector(x = breaks,
                         name = "breaks")
    breaks <- err_tdy_date_vector(x = breaks,
                                  name = "breaks")
    err_strictly_increasing(x = breaks,
                               name = "breaks")
    breaks
}

## HAS_TESTS
#' Check and tidy a vector of breaks used to define periods
#'
#' @inheritParams err_tdy_breaks_date_cohort
#'
#' @seealso \code{\link{err_tdy_breaks_date_cohort}},
#' \code{\link{err_tdy_breaks_integer_age}},
#' \code{\link{err_tdy_breaks_integer_cohort}},
#' \code{\link{err_tdy_lower_upper_enumeration}},
#' \code{\link{err_tdy_breaks_integer_period}}
#'
#' @examples
#' breaks <- c("2000-01-01", "2010-01-01", "2020-01-01")
#' err_tdy_breaks_date_period(breaks)
#' @export
err_tdy_breaks_date_period <- function(breaks) {
    n <- length(breaks)
    if (n == 1L)
        stop(gettextf("'%s' has length %d",
                      "breaks", 1L),
             call. = FALSE)
    err_not_na_vector(x = breaks,
                         name = "breaks")
    breaks <- err_tdy_date_vector(x = breaks,
                                  name = "breaks")
    err_strictly_increasing(x = breaks,
                               name = "breaks")
    breaks
}


## HAS_TESTS
#' Check and tidy a vector of integer breaks used to define age groups
#'
#' @param breaks An integer vector.
#' @param open_last Whether the final interval is open on the right.
#'
#' @seealso \code{\link{err_tdy_breaks_date_cohort}},
#' \code{\link{err_tdy_breaks_date_period}},
#' \code{\link{err_tdy_breaks_integer_cohort}},
#' \code{\link{err_tdy_lower_upper_enumeration}},
#' \code{\link{err_tdy_breaks_integer_period}}
#'
#' @examples
#' breaks <- c(0L, 15L, 65L)
#' err_tdy_breaks_integer_age(breaks,
#'                            open_last = TRUE)
#' @export
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
    err_not_na_vector(x = breaks,
                      name = "breaks")
    err_finite_vector(x = breaks,
                      name = "breaks")
    err_non_negative_vector(x = breaks,
                            name = "breaks")
    err_is_integer_equiv_vector(x = breaks,
                                name = "breaks")
    err_strictly_increasing(x = breaks,
                            name = "breaks")
    as.integer(breaks)
}


## NO_TESTS
#' Check and tidy a vector of integer breaks used to define periods
#'
#' @inheritParams err_tdy_breaks_integer_age
#' @param open_first Whether the first interval is open on the left.
#'
#' @seealso \code{\link{err_tdy_breaks_date_cohort}},
#' \code{\link{err_tdy_breaks_date_period}},
#' \code{\link{err_tdy_breaks_integer_age}},
#' \code{\link{err_tdy_lower_upper_enumeration}},
#' \code{\link{err_tdy_breaks_integer_period}}
#'
#' @examples
#' breaks <- c(2000, 2010, 2020)
#' err_tdy_breaks_integer_cohort(breaks,
#'                               open_first = TRUE)
#' @export
err_tdy_breaks_integer_cohort <- function(breaks, open_first) {
    n <- length(breaks)
    if (n == 0L) {
        if (open_first)
            stop(gettextf("'%s' has length %d but '%s' is %s",
                          "breaks", 0L, "open_first", "TRUE"))
    }
    if (n == 1L) {
        if (!open_first)
            stop(gettextf("'%s' has length %d but '%s' is %s",
                          "breaks", 1L, "open_first", "FALSE"))
    }
    err_not_na_vector(x = breaks,
                      name = "breaks")
    err_finite_vector(x = breaks,
                      name = "breaks")
    err_is_integer_equiv_vector(x = breaks,
                                name = "breaks")
    err_strictly_increasing(x = breaks,
                            name = "breaks")
    as.integer(breaks)
}


## HAS_TESTS
#' Check and tidy 'lower' and 'upper' vectors used to
#' define enumerations
#'
#' The first element of 'lower' can be \code{NA},
#' implying that the first interval is open.
#' The last element of 'upper' can be \code{NA},
#' implying that the last interval is open.
#'
#' There can be gaps between intervals, but intervals
#' must not overlap. The width of every interval
#' must be at least 1.
#' 
#' @param lower A vector that can be coerced to integer.
#' @param upper A vector that can be coerced to integer.
#'
#' @seealso \code{\link{err_tdy_breaks_date_cohort}},
#' \code{\link{err_tdy_breaks_date_period}},
#' \code{\link{err_tdy_breaks_integer_age}},
#' \code{\link{err_tdy_breaks_integer_cohort}},
#' \code{\link{err_tdy_breaks_integer_period}}
#'
#' @examples
#' lower <- c(NA, 0, 5, 20)
#' upper <- c(0, 5, 10, NA)
#' err_tdy_lower_upper_enumeration(lower = lower,
#'                                 upper = upper)
#' @export
err_tdy_lower_upper_enumeration <- function(lower, upper) {
    err_length_same(x1 = lower,
                    x2 = upper,
                    name1 = "lower",
                    name2 = "upper")
    n <- length(lower)
    if (identical(n, 0L)) {
        ans <- list(lower = integer(),
                    upper = integer())
        return(ans)
    }
    err_not_na_vector(x = lower[-1L], ## allow for first interval open
                      name = "lower")
    err_not_na_vector(x = upper[-n], ## allow for last interval open
                      name = "upper")
    for (name in c("lower", "upper")) {
        x <- get(name)
        err_finite_vector(x = x[!is.na(x)],
                          name = name)
        err_is_integer_equiv_vector(x = x[!is.na(x)],
                                    name = name)
        err_strictly_increasing(x = x[!is.na(x)],
                                name = name)
        assign(x = name, value = as.integer(x))
    }
    for (i in 1:n) {
        low <- lower[[i]]
        up <- upper[[i]]
        if (!is.na(low) && !is.na(up) && (up <= low))
            stop(gettextf("element %d of '%s' [%d] less than or equal to element %d of '%s' [%d]",
                          i, "upper", up, i, "lower", low),
                 call. = FALSE)
    }
    if (n >= 2L) {
        for (i in 2:n) {
            low <- lower[[i]]
            up <- upper[[i - 1L]]
            if (low < up)
                stop(gettextf("element %d of '%s' [%d] less than element %d of '%s' [%d]",
                              i, "lower", low, i - 1L, "upper", up),
                     call. = FALSE)
        }
    }
    ans <- list(lower = lower,
                upper = upper)
    ans
}


## NO_TESTS
#' Check and tidy a vector of integer breaks used to define periods
#'
#' @inheritParams err_tdy_breaks_integer_age
#'
#' @seealso \code{\link{err_tdy_breaks_date_cohort}},
#' \code{\link{err_tdy_breaks_date_period}},
#' \code{\link{err_tdy_breaks_integer_age}},
#' \code{\link{err_tdy_breaks_integer_cohort}},
#' \code{\link{err_tdy_lower_upper_enumeration}}
#'
#' @examples
#' breaks <- c(2000, 2010, 2020)
#' err_tdy_breaks_integer_period(breaks)
#' @export
err_tdy_breaks_integer_period <- function(breaks) {
    n <- length(breaks)
    if (n == 1L) {
        stop(gettextf("'%s' has length %d",
                      "breaks", 1L))
    }
    err_not_na_vector(x = breaks,
                      name = "breaks")
    err_finite_vector(x = breaks,
                      name = "breaks")
    err_is_integer_equiv_vector(x = breaks,
                                name = "breaks")
    err_strictly_increasing(x = breaks,
                            name = "breaks")
    as.integer(breaks)
}


## HAS_TESTS
#' Check and tidy a date scalar or vector
#'
#' @param x A scalar or vector of class \code{\link[base:Dates]{Date}},
#' or something that can be coerced to one.
#' @param name The name for \code{x} that
#' will be used in error messages.
#'
#' @examples
#' x <- "2001-03-01"
#' err_tdy_date_scalar(x, name = "x")
#' x <- c("2000-05-01", "2000-06-01", "2000-07-01")
#' err_tdy_date_vector(x, name = "x")
#' @export
#' @name err_tdy_date_scalar
NULL

#' @export
#' @rdname err_tdy_date_scalar
err_tdy_date_scalar <- function(x, name) {
    err_length_1(x = x,
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

#' @export
#' @rdname err_tdy_date_scalar
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
#' Check and tidy dates and birth dates
#'
#' @param date Dates when measurements made or events occurred.
#' A scalar or vector of class \code{\link[base:Dates]{Date}},
#' or something that can be coerced to one.
#' @param dob Dates of birth.
#' A scalar or vector of class \code{\link[base:Dates]{Date}},
#' or something that can be coerced to one. Must have
#' same length as \code{date}.
#'
#' @examples
#' date <- c("2000-05-01", "2000-06-01", "2000-07-01")
#' dob <- c("1999-03-21", "1980-12-22", "1990-04-23")
#' err_tdy_date_dob(date = date, dob = dob)
#' @export
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
    err_ge_vector(x1 = date,
                     x2 = dob,
                     name1 = "date",
                     name2 = "dob")
    list(date = date,
         dob = dob)
}


#' Check and tidy integers or objects that can be coerced to integer
#'
#' @inheritParams err_tdy_date_scalar
#' @param x A scalar or vector of integers, or quantities
#' that can be coerced to integer
#' @param null_ok Whether to allow \code{x} to be \code{NULL}.
#'
#' @seealso \code{\link{err_tdy_date_scalar}}
#'
#' @examples
#' x <- 1.0
#' err_tdy_integer_scalar(x = x, name = "x", null_ok = FALSE)
#' x <- NULL
#' err_tdy_integer_scalar(x = x, name = "x", null_ok = TRUE)
#' x <- 3:1
#' err_tdy_integer_vector(x = x, name = "x")
#' @name err_tdy_integer
NULL

## HAS_TESTS
#' @export
#' @rdname err_tdy_integer
err_tdy_integer_scalar <- function(x, name, null_ok) {
    if (is.null(x)) {
        if (null_ok)
            return(x)
        else
            stop(gettextf("'%s' is %s",
                          name, "NULL"),
                 call. = FALSE)
    }
    err_length_1(x = x,
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
#' @rdname err_tdy_integer
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
#' Check and tidy a data frame describing a many-to-one mapping
#' 
#' @inheritParams err_tdy_date_scalar
#' @param x A data frame.
#'
#' @examples
#' x <- data.frame(from = 1:3,
#'                 to = c("A", "B", "A"))
#' err_tdy_many_to_one(x, name = "x")
#' @export
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
    err_not_na_dataframe(x = x,
                            name = name)
    is_unique <- sapply(x, function(x) !any(duplicated(x)))
    if (all(is_unique))
        stop(gettextf("neither column of '%s' has duplicates, as required for many-to-one mapping",
                      name),
             call. = FALSE)
    if (!any(is_unique))
        stop(gettextf("neither column of '%s' has entirely unique values, as required for many-to-one mapping",
                      name),
             call. = FALSE)
    x[] <- lapply(x, as.character)
    x        
}


## HAS_TESTS
#' Check and tidy 'map_dim'
#'
#' Check and tidy \code{map_dim}, a mapping between the
#' dimensions of arrays \code{self} and \code{oth}.
#' If a dimension of \code{self}
#' does not map on to a dimension of \code{oth}, then the
#' corresponding element of \code{map_dim} is zero.
#' \code{oth} can have dimensions that \code{self}
#' does not map on to, so \code{map_dim} does not
#' necessarily contain every number in
#' \code{seq_along(dim_oth)}.
#'
#' \code{self} and \code{oth} are both assumed to have positive
#' length, ie no dimensions with length 0.
#'
#' @param map_dim A vector of non-negative integers,
#' unique apart from any zeros, the same length as
#' \code{dim_self}.
#' @param n_dim_self The number of dimensions of \code{self}.
#' @param n_dim_oth The number of dimensions of \code{oth}.
#'
#' @return \code{map_dim}, coerced to integer.
#'
#' @seealso \code{\link{err_tdy_map_pos}},
#' \code{\link{chk_map_dim}}
#'
#' @examples
#' err_tdy_map_dim(map_dim = c(3, 1, 0),
#'                 n_dim_self = 3L,
#'                 n_dim_oth = 4L)
#' @export
err_tdy_map_dim <- function(map_dim, n_dim_self, n_dim_oth) {
    map_dim <- err_tdy_non_negative_integer_vector(x = map_dim,
                                                   name = "map_dim")
    err_has_nonzero(x = map_dim,
                    name = "map_dim")
    err_nonzero_unique(x = map_dim,
                       name = "map_dim")
    err_length_equals(x1 = map_dim,
                      x2 = n_dim_self,
                      name1 = "map_dim",
                      name2 = "n_dim_self")
    err_all_x1_in_x2(x1 = map_dim,
                     x2 = seq_len(n_dim_oth),
                     name1 = "map_dim",
                     name2 = "seq_len(n_dim_oth)",
                     exclude_zero = TRUE)
    map_dim
}


## HAS_TESTS
#' Check and tidy 'map_pos'
#'
#' Check and tidy \code{map_pos}, a mapping between
#' positions along dimensions of array \code{self} and
#' positions along dimensions of array \code{oth}.
#' \code{map_pos} is a list the same length as
#' \code{dim_self}. Each element of \code{map_pos}
#' is a vector, showing, for each position on the dimension
#' of \code{self}, which postition on the dimension
#' of \code{oth} it maps on to. Multiple positions
#' on \code{self} can map on to the same position on \code{oth}.
#' If a position on \code{self} does not map to a position,
#' on \code{oth}, the value for the non-mapping position
#' is \code{0}. If a dimension of \code{self}
#' does not have a corresponding dimension in
#' \code{oth}, then corresponding element of \code{map_dim}
#' is vector of zeros.
#'
#' \code{err_tdy_map_pos} assumes that \code{map_dim},
#' \code{dim_self}, and \code{dim_oth} are all valid.
#'
#' \code{self} and \code{oth} are both assumed to have positive
#' length, ie no dimensions with length 0.
#'
#' @param map_pos A list of integer vectors.
#' @param map_dim A vector of non-negative integers.
#' @param dim_self The dimensions of \code{self}.
#' @param dim_oth The dimensions of \code{oth}.
#'
#' @return \code{map_pos}, with elements coerced to integer.
#'
#' @seealso \code{\link{err_tdy_map_dim}}
#'
#' @examples
#' map_pos <- list(c(1L, 2L, 3L, 4L), c(0L, 1L), c(1L, 2L, 2L))
#' map_dim <- c(1L, 3L, 2L)
#' dim_self <- c(4L, 2L, 3L)
#' dim_oth <- c(4L, 2L, 1L)
#' err_tdy_map_pos(map_pos = map_pos,
#'                 map_dim = map_dim,
#'                 dim_self = dim_self,
#'                 dim_oth = dim_oth)
#'
#' map_pos <- list(c(2L, 1L), c(1L, 1L), c(0L, 0L, 0L))
#' map_dim <- c(1L, 2L, 0L)
#' dim_self <- c(2L, 2L, 3L)
#' dim_oth <- c(2L, 1L)
#' err_tdy_map_pos(map_pos = map_pos,
#'                 map_dim = map_dim,
#'                 dim_self = dim_self,
#'                 dim_oth = dim_oth)
#' @export
err_tdy_map_pos <- function(map_pos, dim_self, dim_oth, map_dim) {
    ## 'map_pos' same length as 'dim_self'
    err_length_same(x1 = map_pos,
                    x2 = dim_self,
                    name1 = "map_pos",
                    name2 = "dim_self")
    ## lengths of entries of 'map_pos' consistent with 'dim_self'
    err_lengths_elements_equal_vec(x1 = map_pos,
                                   x2 = dim_self,
                                   name1 = "map_pos",
                                   name2 = "dim_self")
    for (i_dim_self in seq_along(dim_self)) {
        val_map_pos <- map_pos[[i_dim_self]]
        name_val_map_pos <- gettextf("element %d of '%s'",
                                     i_dim_self, "map_pos")
        val_map_pos <- err_tdy_non_negative_integer_vector(x = val_map_pos,
                                                           name = name_val_map_pos)
        i_dim_oth <- map_dim[[i_dim_self]]
        is_dim_in_oth <- i_dim_oth > 0L
        if (is_dim_in_oth) {
            val_dim_oth <- dim_oth[[i_dim_oth]]
            s_dim_oth <- seq_len(val_dim_oth)
            name_s_dim_oth <- gettextf("seq_len(dim_oth[[%d]])", i_dim_oth)
            err_all_x1_in_x2(x1 = val_map_pos,
                             x2 = s_dim_oth,
                             name1 = name_val_map_pos,
                             name2 = name_s_dim_oth,
                             exclude_zero = TRUE)
            err_all_x1_in_x2(x1 = s_dim_oth,
                             x2 = val_map_pos,
                             name1 = name_s_dim_oth,
                             name2 = name_val_map_pos,
                             exclude_zero = FALSE)
        }
        else {
            if (any(val_map_pos != 0L)) {
                stop(gettextf("dimension %d of '%s' does not map on to '%s', but %s has non-zero elements",
                              i_dim_self, "self", "oth", name_val_map_pos),
                     call. = FALSE)
            }
        }
        map_pos[[i_dim_self]] <- val_map_pos
    }
    map_pos
}

#' Check and tidy numeric or character vector
#' of months
#'
#' Convert a numeric or character vector of month
#' indices or codes to an integer vector.
#' The vector can contain NAs.
#'
#' @inheritParams err_tdy_date_scalar
#' @param x A numeric or character vector.
#'
#' @examples
#' err_tdy_month(x = c(1, NA, 12, 5),
#'               name = "x")
#' err_tdy_month(x = c("01", NA, "12", "05"),
#'               name = "x")
#' err_tdy_month(x = c("Jan", NA, "Dec", "May"),
#'               name = "x")
#' err_tdy_month(x = c("January", NA, "December", "May"),
#'               name = "x")
#' @export
err_tdy_month <- function(x, name) {
    is_obs <- !is.na(x)
    x_obs <- x[is_obs]
    ## 1, 2, ..., 12
    if (is.numeric(x)) {
        ans <- demcheck::err_tdy_integer_vector(x = x,
                                                name = name)
        err_integer_in_range(x = x,
                             min = 1L,
                             max = 12L,
                             name = name)
    }
    else if (is.character(x)) {
        ## "01", "02", ..., "12"
        tab_2digit <- sprintf("%02.0f", 1:12)
        ans <- match(x, tab_2digit, nomatch = 0L)
        is_invalid <- is_obs & (ans == 0L)
        if (any(is_invalid)) {
            ## "Jan", "Feb", ..., "Dec"
            ans <- match(x, month.abb, nomatch = 0L)
            is_invalid <- is_obs & (ans == 0L)
            if (any(is_invalid)) {
                ## "January", "February", ..., "December"
                ans <- match(x, month.name, nomatch = 0L)
                is_invalid <- is_obs & (ans == 0L)
                if (any(is_invalid))
                    stop(gettextf("elements of '%s' cannot be interpreted as codes or names of months",
                                  name))
            }
        }
    }
    else {
        stop(gettextf("'%s' has class \"%s\"",
                      name, class(x)))
    }
    ans[!is_obs] <- NA_integer_
    ans
}


## HAS_TESTS
#' Check and tidy 'month_start' argument,
#' used in defining years
#'
#' @inheritParams err_tdy_date_scalar
#' @param x An abbreviated month name.
#'
#' @examples
#' x <- "Feb"
#' err_tdy_month_start(x = x, name = "x")
#' @export
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
#' Check and tidy a non-negative integer scalar or vector
#'
#' @inheritParams err_tdy_date_scalar
#' @param x An integer scalar or vector, or somthing that
#' can be coerced to one.
#' @param null_ok Whether \code{x} can be \code{NULL}.
#'
#' @seealso \code{\link{err_tdy_positive_integer}}
#'
#' @examples
#' x <- 3.0
#' err_tdy_non_negative_integer_scalar(x = x, name = "x", null_ok = FALSE)
#' x <- NULL
#' err_tdy_non_negative_integer_scalar(x = x, name = "x", null_ok = TRUE)
#' x <- c(3.0, 0.0, 4.0)
#' err_tdy_non_negative_integer_vector(x = x, name = "x")
#' @name err_tdy_non_negative_integer
NULL

#' @rdname err_tdy_non_negative_integer
#' @export
err_tdy_non_negative_integer_scalar <- function(x, name, null_ok) {
    if (is.null(x)) {
        if (null_ok)
            return(x)
        else
            stop(gettextf("'%s' is %s",
                          name, "NULL"),
                 call. = FALSE)
    }
    err_non_negative_scalar(x = x,
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

#' @rdname err_tdy_non_negative_integer
#' @export
err_tdy_non_negative_integer_vector <- function(x, name) {
    err_non_negative_vector(x = x,
                            name = name)
    if (is.integer(x))
        return(x)
    x_int <- suppressWarnings(as.integer(x))
    is_not_equiv <- is.na(x_int) | (x_int != x)
    i_not_equiv <- match(TRUE, is_not_equiv, nomatch = 0L)
    if (i_not_equiv > 0L)
        stop(gettextf("element %d of '%s' [%s] not equivalent to integer",
                      i_not_equiv, name, x),
             call. = FALSE)
    x_int
}



## HAS_TESTS
#' Check and tidy a non-negative integer scalar or vector
#'
#' @inheritParams err_tdy_date_scalar
#' @param x An integer scalar or vector, or somthing that
#' can be coerced to one.
#' @param null_ok Whether \code{x} can be \code{NULL}.
#' 
#' @seealso \code{\link{err_tdy_non_negative_integer_scalar}}
#'
#' @examples
#' x <- 3.0
#' err_tdy_positive_integer_scalar(x = x, name = "x")
#' x <- NULL
#' err_tdy_positive_integer_scalar(x = x, name = "x", null_ok = TRUE)
#' x <- c(3.0, 1.0, 4.0)
#' err_tdy_positive_integer_vector(x = x, name = "x")
#' @name err_tdy_positive_integer
NULL

#' @rdname err_tdy_positive_integer
#' @export
err_tdy_positive_integer_scalar <- function(x, name, null_ok) {
    if (is.null(x)) {
        if (null_ok)
            return(x)
        else
            stop(gettextf("'%s' is %s",
                          name, "NULL"),
                 call. = FALSE)
    }
    err_positive_scalar(x = x,
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

#' @rdname err_tdy_positive_integer
#' @export
err_tdy_positive_integer_vector <- function(x, name) {
    err_positive_vector(x = x,
                        name = name)
    if (is.integer(x))
        return(x)
    x_int <- suppressWarnings(as.integer(x))
    is_not_equiv <- is.na(x_int) | (x_int != x)
    i_not_equiv <- match(TRUE, is_not_equiv, nomatch = 0L)
    if (i_not_equiv > 0L)
        stop(gettextf("element %d of '%s' [%s] not equivalent to integer",
                      i_not_equiv, name, x),
             call. = FALSE)
    x_int
}


## HAS_TESTS
#' Ensure that two vectors have same length,
#' if necessary by replicating a length-1 vector
#'
#' @param x1 An object.
#' @param x2 An object.
#' @param name1 The name for \code{x1} that
#' will be used in error messages.
#' @param name2 The name for \code{x2} that
#' will be used in error messages.
#'
#' @seealso \code{\link{chk_length_same_or_1}}
#'
#' @examples
#' x1 <- 1:5
#' x2 <- 1
#' err_tdy_same_length(x1 = x1, x2 = x2,
#'                     name1 = "x1", name2 = "x2")
#' @export
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


## HAS_TESTS
#' Check and tidy time unit
#'
#' @inheritParams err_tdy_date_scalar
#' @param x A string.
#'
#' @seealso \code{\link{chk_member_unit}}
#' 
#' @examples
#' x <- "5 years"
#' err_tdy_unit(x, name = "x")
#' @export
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
                  name, x),
         call. = FALSE)
}

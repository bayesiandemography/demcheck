
## Checks of composite conditions
##
## Functions to check for conditions that involve
## several attributes. For instance, \code{chk_integer_consec}
## checks whether \code{x} has type integer, whether \code{x}
## has no \code{NA}s, and whether \code{all(diff(x)) == 1L}.


## HAS_TESTS
#' Check whether all elements of 'x1' are in 'x2'
#'
#' Check \code{x1} is contained in \code{x2}, potentially
#' after excluding \code{0}s from \code{x1}.
#'
#' The error message for \code{chk_all_x1_in_x2} does not
#' have quotes around the arguments, since these arguments
#' are often expressions.
#'
#' @param x1 A vector, typically integers.
#' @param x2 A vector, typically integers.
#' @param name1 The name for \code{x1} that
#' will be used in error messages.
#' @param name2 The name for \code{x2} that
#' will be used in error messages.
#' @param exclude_zero Whether to exclude zeros
#' from \code{x1} before testing.
#' 
#' @examples
#' x1 <- c(0L, 1L, 2L, 0L)
#' x2 <- 1:3
#' chk_all_x1_in_x2(x1 = x2,
#'                  x2 = x2,
#'                  name1 = "x1",
#'                  name2 = "x2",
#'                  exclude_zero = TRUE)
#' @export
chk_all_x1_in_x2 <- function(x1, x2, name1, name2, exclude_zero) {
    if (exclude_zero)
        x1 <- x1[x1 != 0L]
    i_x2 <- match(x1, x2)
    i_miss <- match(NA_integer_, i_x2, nomatch = 0L)
    if (i_miss > 0L)
        return(gettextf("element from %s not found in %s : %d",
                        name1, name2, x1[[i_miss]]))
    TRUE    
}


## HAS_TESTS
#' Check that an array has complete metadata
#'
#' Check it is possible to uniquely identify every cell
#' in an array, using the array's dimnames and the
#' the names of the dimnames.
#'
#' @param x An array.
#' @param name The name for \code{x} that
#' will be used in error messages.
#' 
#' @examples
#' x <- array(1:6,
#'            dim = 3:2,
#'            dimnames = list(region = c("a", "b", "c"),
#'                            sex = c("Female", "Male")))
#' chk_array_metadata_complete(x, name = "x")
#' @export
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
#' Check that a character vector has no
#' NAs, blanks, or duplicates
#'
#' @inheritParams chk_array_metadata_complete
#' @param x A character vector.
#'
#' @examples
#' x <- c("A", "B", "C")
#' chk_character_complete(x, name = "x")
#' @export
chk_character_complete <- function(x, name) {
    if (any(is.na(x)))
        return(gettextf("'%s' has NAs",
                        name))
    if (!all(nzchar(x)))
        return(gettextf("'%s' has blanks",
                        name))
    is_duplicated <- duplicated(x)
    if (any(is_duplicated)) {
        i <- match(TRUE, is_duplicated)
        return(gettextf("'%s' has duplicate [\"%s\"]",
                        name, x[[i]]))
    }
    TRUE
}


## HAS_TESTS
#' Check that all dimensions of an array have complete dimnames
#'
#' Check that the dimnames for an array include every
#' dimension, and have no blanks, NAs, or duplicates
#'
#' @param x An array.
#' @param name The name for \code{x} that
#' will be displayed in error messages.
#'
#' @seealso \code{\link{chk_array_metadata_complete}}.
#' 
#' @examples
#' x <- array(1:6,
#'            dim = 3:2,
#'            dimnames = list(region = c("a", "b", "c"),
#'                            sex = c("Female", "Male")))
#' chk_dimnames_complete(x, name = "x")
#' @export
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
            n_na <- sum(is.na(dimnames_i))
            if (n_na > 1L)
                return(gettextf("dimnames for \"%s\" dimension of '%s' have %d NAs",
                                names_i, name, n_na))
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
#' Check that dimtypes obey rules about appearing at most once,
#' or not occurring with other dimtypes
#'
#' @param dimtypes A character vector of dimtypes.
#'
#' @seealso \code{\link{chk_names_pairs_complete}}
#'
#' @examples
#' dimtypes <- c("age", "time", "triangle")
#' chk_dimtypes_mutually_compatible(dimtypes)
#' @export
chk_dimtypes_mutually_compatible <- function(dimtypes) {
    at_most_one <- c("age",
                     "time",
                     "cohort",
                     "triangle",
                     "iteration",
                     "quantile")
    for (dimtype in at_most_one) {
        if (sum(dimtypes == dimtype) > 1L)
            return(gettextf("two dimensions with dimtype \"%s\"",
                            dimtype))
    }
    if (("iteration" %in% dimtypes) && ("quantile" %in% dimtypes))
        return(gettextf("dimension with dimtype \"%s\" and dimension with dimtype \"%s\"",
                        "iteration", "quantile"))
    TRUE
}

## HAS_TESTS
#' Check that dimensions that should have pairs do
#' in fact have them
#'
#' Function \code{\link{chk_names_pairs_suffix}}
#' should be run before this one.
#'
#' @param names A character vector of dimension names.
#'
#' @examples
#' names <- c("reg_dest", "reg_orig")
#' chk_names_pairs_complete(names)
#' @export
chk_names_pairs_complete <- function(names) {
    vals <- list(c("origin", "_orig$", "destination", "_dest"),
                 c("destination", "_dest$", "origin", "_orig"),
                 c("parent", "_parent$", "child", "_child"),
                 c("child", "_child$", "parent", "_parent"))
    for (val in vals) {
        dimtype <- val[[1L]]
        p_dimtype <- val[[2L]]
        pair <- val[[3L]]
        p_pair <- val[[4L]]
        is_dimtype <- grepl(p_dimtype, names)
        if (any(is_dimtype)) {
            names_pair_implied <- sub(p_dimtype, p_pair, names[is_dimtype])
            is_not_found <- !(names_pair_implied %in% names)
            i_not_found <- match(TRUE, is_not_found, nomatch = 0L)
            if (i_not_found > 0L) {
                return(gettextf(paste("dimension \"%s\" with dimtype \"%s\" does not",
                                      "have paired dimension \"%s\" with dimtype \"%s\""),
                                names[is_dimtype][[i_not_found]],
                                dimtype,
                                names_pair_implied[[i_not_found]],
                                pair))
            }
        }
    }
    TRUE
}


## HAS_TESTS
#' Check that dimension names have pair suffixes if and only if
#' they have the appropriate dimtypes
#'
#' @param dimtypes A character vector of dimtypes.
#' @param names A character vector of names.
#'
#' @seealso Function \code{\link{chk_names_pairs_complete}} should only
#' be called if \code{chk_names_pairs_suffix} has been
#' called first
#'
#' @examples
#' dimtypes <- c("origin", "destination")
#' names <- c("reg_orig", "reg_dest")
#' chk_names_pairs_suffix(dimtypes = dimtypes, names = names)
#' @export
chk_names_pairs_suffix <- function(dimtypes, names) {
    patterns <- c(origin = "_orig",
                  destination = "_dest",
                  parent = "_parent",
                  child = "_child")
    for (i in seq_along(patterns)) {
        dimtype <- names(patterns)[[i]]
        pattern <- patterns[[i]]
        is_dimtype <- grepl(dimtype, dimtypes)
        is_pattern <- grepl(paste0(pattern, "$"), names)
        i_dimtype_not_pattern <- match(TRUE, is_dimtype & !is_pattern, nomatch = 0L)
        if (i_dimtype_not_pattern > 0L)
            return(gettextf("dimension \"%s\" has dimtype \"%s\" but name does not end with \"%s\"",
                            names[[i_dimtype_not_pattern]], dimtype, pattern))
        i_pattern_not_dimtype <- match(TRUE, is_pattern & !is_dimtype, nomatch = 0L)
        if (i_pattern_not_dimtype > 0L)
            return(gettextf("dimension \"%s\" has name ending with \"%s\" but does not have dimtype \"%s\"",
                            names[[i_pattern_not_dimtype]], pattern, dimtype))
    }
    TRUE   
}


## HAS_TESTS
#' Check that implied ages are greater than or equal to 'break_min'
#'
#' @param age Integer. Age, typically in years, but can be other unit.
#' @param break_min Integer or NULL. If non-NULL, lowest permissible value
#' for \code{age}.
#' @param date Date on which event occurred or measurement made.
#' Object of class \code{\link[base:Dates]{Date}}.
#' @param dob Date of birth. Object of class \code{\link[base:Dates]{Date}}.
#' @param unit Measurement units for age, eg \code{"month"}.
#' 
#' @seealso \code{\link{chk_ge_break_min_date}},
#' \code{\link{chk_lt_break_max_age}},
#' \code{\link{chk_lt_break_max_date}}
#'
#' @examples
#' age <- c(20L, 23L)
#' break_min <- 15L
#' date <- as.Date(c("2020-03-17", "2020-03-18"))
#' dob <- as.Date(c("2000-02-01", "1997-01-13"))
#' unit <- "years"
#' chk_ge_break_min_age(age = age,
#'                      break_min = break_min,
#'                      date = date,
#'                      dob = dob,
#'                      unit = unit)
#' @export
chk_ge_break_min_age <- function(age, break_min, date, dob, unit) {
    if (is.null(break_min))
        return(TRUE)
    lt_min <- !is.na(age) & (age < break_min)
    i <- match(TRUE, lt_min, nomatch = 0L)
    if (i > 0L) {
        return(gettextf(paste("'%s' [\"%s\"] and '%s' [\"%s\"] imply an age of %d %ss,",
                              "which is less than '%s' [%d %ss]"),
                        "date",
                        date[[i]],
                        "dob",
                        dob[[i]],
                        age[[i]],
                        unit,
                        "break_min",
                        break_min,
                        unit))
    }
    TRUE
}

## HAS_TESTS
#' Check that dates are greater than or equal to 'break_min'
#'
#' @param break_min Date or NULL. If non-NULL, lowest permissible value
#' for \code{date}.
#' @param date Date on which event occurred or measurement made.
#' Object of class \code{\link[base:Dates]{Date}}.
#' 
#' @seealso \code{\link{chk_ge_break_min_age}},
#' \code{\link{chk_lt_break_max_age}}
#' \code{\link{chk_lt_break_max_date}}
#' 
#' @examples
#' date <- as.Date(c("2020-03-17", "2020-03-18"))
#' break_min <- as.Date("2020-01-01")
#' chk_ge_break_min_date(date = date,
#'                       break_min = break_min)
#' @export
chk_ge_break_min_date <- function(date, break_min) {
    if (is.null(break_min))
        return(TRUE)
    lt_date_min <- !is.na(date) & (date < break_min)
    i <- match(TRUE, lt_date_min, nomatch = 0L)
    if (i > 0L) {
        return(gettextf("'%s' has value [\"%s\"] that is less than '%s' [\"%s\"]",
                        "date",
                        date[[i]],
                        "break_min",
                        break_min))
    }
    TRUE
}


#' Check that object or elements of list inherit
#' from specified class
#'
#' @inheritParams chk_array_metadata_complete
#' @param x An object, or a list of objects
#' @param class Name of a class.
#'
#' @examples
#' x <- "a"
#' class <- "character"
#' chk_is_class_obj(x, name = "x", class = class)
#' x <- list(c("a", "b"), "c")
#' chk_is_class_list(x, name = "x", class = class)
#' @name chk_is_class
NULL

## HAS_TESTS
#' @export
#' @rdname chk_is_class
chk_is_class_obj <- function(x, name, class) {
    if (!methods::is(x, class)) {
        return(gettextf(paste("'%s' has class \"%s\" :",
                              "should instead inherit from class \"%s\""),
                        name, class(x), class))
    }
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname chk_is_class
chk_is_class_list <- function(x, name, class) {
    for (i in seq_along(x)) {
        if (!methods::is(x[[i]], class))
            return(gettextf(paste("element %d of '%s' has class \"%s\" :",
                                  "should instead inherit from class \"%s\""),
                            i, name, class(x[[i]]), class))
    }
    TRUE
}



#' Check dates are the first day of the specified unit
#'
#' If \code{unit} is \code{"months"},
#' then \code{"2000-03-01"} is the first day of the specified
#' unit, but \code{"2000-03-02"} is not.
#'
#' \code{chk_first_day_unit_consec} adds the requirement
#' that the dates be consecutive.
#'
#' @inheritParams chk_array_metadata_complete
#' @param x A scalar or vector of dates, or values
#' that can be coerced to dates.
#' @param unit Measurement units for time, eg \code{"month"}.
#'
#' @seealso \code{\link{chk_is_date_equiv}}
#'
#' @examples
#' x <- "2020-01-01"
#' chk_first_day_unit_scalar(x, name = "x", unit = "year")
#' x <- c("2020-01-01", "2020-02-01")
#' chk_first_day_unit_vector(x, name = "x", unit = "month")
#' chk_first_day_unit_consec(x, name = "x", unit = "month")
#' @name chk_first_day_unit_scalar
NULL

## HAS_TESTS
#' @export
#' @rdname chk_first_day_unit_scalar
chk_first_day_unit_scalar <- function(x, name, unit) {
    val <- chk_length_1(x = x,
                           name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_not_na_scalar(x = x,
                                name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_is_date_equiv_scalar(x = x,
                                    name = name)
    if (!isTRUE(val))
        return(val)
    x <- as.Date(x)
    val <- chk_member_unit(x = unit,
                           name = "unit")
    if (!isTRUE(val))
        return(val)
    year <- as.integer(format(x, "%Y"))
    unit_is_year <- identical(unit, "year")
    if (unit_is_year) {
        month <- format(x, "%m")
        from <- as.Date(sprintf("%d-%s-01", year, month))
        to <- as.Date(sprintf("%d-%s-01", year + 1L, month))
    }
    else {
        from <- as.Date(sprintf("%d-01-01", year))
        to <- as.Date(sprintf("%d-01-01", year + 1L))
    }
    seq_expected <- seq.Date(from = from,
                             to = to,
                             by = unit)
    is_not_in_seq <- !(x %in% seq_expected)
    if (is_not_in_seq) {
        msg <- gettextf("'%s' [\"%s\"] is not the first day of the %s",
                        name, format(x, "%Y-%m-%d"), unit)
        if (unit_is_year)
            msg <- paste(msg,
                         gettextf("(years assumed to start on the first day of %s)",
                                  format(x, "%B")))
        return(msg)        
    }
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname chk_first_day_unit_scalar
chk_first_day_unit_vector <- function(x, name, unit) {
    val <- chk_not_na_vector(x = x,
                                name = name)
    n <- length(x)
    if (n == 0L)
        return(TRUE)
    val <- chk_not_na_vector(x = x,
                                name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_is_date_equiv_vector(x = x,
                                    name = name)
    if (!isTRUE(val))
        return(val)
    x <- as.Date(x)
    val <- chk_member_unit(x = unit,
                           name = "unit")
    if (!isTRUE(val))
        return(val)
    year <- as.integer(format(x, "%Y"))
    unit_is_year <- identical(unit, "year")
    if (unit_is_year) {
        month <- format(x, "%m")
        month_start <- month[[1L]]
        from <- as.Date(sprintf("%d-%s-01", min(year), month_start))
        to <- as.Date(sprintf("%d-%s-01", max(year) + 1L, month_start))
    }
    else {
        from <- as.Date(sprintf("%d-01-01", min(year)))
        to <- as.Date(sprintf("%d-01-01", max(year) + 1L))
    }
    seq_expected <- seq.Date(from = from,
                             to = to,
                             by = unit)
    is_not_in_seq <- !(x %in% seq_expected)
    i_not_in_seq <- match(TRUE, is_not_in_seq, nomatch = 0L)
    if (i_not_in_seq > 0L) {
        msg <- gettextf("element %d [\"%s\"] of '%s' is not the first day of the %s",
                        i_not_in_seq,
                        format(x[[i_not_in_seq]], "%Y-%m-%d"),
                        name,
                        unit)
        if (unit_is_year)
            msg <- paste(msg,
                         gettextf("(years assumed to start on the first day of %s)",
                                  format(x[[1L]], "%B")))
        return(msg)        
    }
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname chk_first_day_unit_scalar
chk_first_day_unit_consec <- function(x, name, unit) {
    n <- length(x)
    if (n == 0L)
        return(TRUE)
    val <- chk_first_day_unit_vector(x = x,
                                     name = name,
                                     unit = unit)
    if (!isTRUE(val))
        return(val)
    x_date <- as.Date(x)
    if (n >= 2L) {
        from <- x_date[[1L]]
        seq_expected <- seq.Date(from = from,     # Calculation using 'seq.Date' relies
                                 by = unit,       # on each date being the first day
                                 length.out = n)  # of the month
        is_not_equal_to_seq <- x_date != seq_expected
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


#' Check that 'x1' is greater than or equal to 'x2'
#'
#' @inheritParams chk_all_x1_in_x2
#' @param x1 A scalar or vector.
#' @param x2 A scalar or vector the same length as \code{x1},
#' or length 1.
#'
#' @seealso \code{\link{chk_gt}}, \code{\link{chk_le}},
#' \code{\link{chk_lt}}
#'
#' @examples
#' x1 <- 3.1
#' x2 <- 3.1
#' chk_ge_scalar(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' x1 <- c(3.1, 4.2, 5.7)
#' x2 <- c(3.1, 4.0, 4.2)
#' chk_ge_vector(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' x1 <- c(3.1, 4.2, 5.7)
#' x2 <- 3.1
#' chk_ge_vector(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' @name chk_ge
NULL

## HAS_TESTS
#' @export
#' @rdname chk_ge
chk_ge_scalar <- function(x1, x2, name1, name2) {
    if (!is.na(x1) && !is.na(x2) && (x1 < x2))
        return(gettextf("'%s' [%s] is less than '%s' [%s]",
                        name1, x1, name2, x2))
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname chk_ge
chk_ge_vector <- function(x1, x2, name1, name2) {
    n1 <- length(x1)
    n2 <- length(x2)
    if ((n2 != n1) && (n2 != 1L))
        return(gettextf("'%s' has length %d and '%s' has length %d",
                        name1, n1, name2, n2))
    is_lt <- !is.na(x1) & !is.na(x2) & (x1 < x2)
    if (any(is_lt)) {
        i <- match(TRUE, is_lt)
        if (n2 > 1L)
            return(gettextf("element %d of '%s' [%s] is less than element %d of '%s' [%s]",
                            i, name1, x1[[i]], i, name2, x2[[i]]))
        else
            return(gettextf("element %d of '%s' [%s] is less than '%s' [%s]",
                            i, name1, x1[[i]], name2, x2))
    }
    TRUE
}


#' Check that 'x1' is greater than 'x2'
#'
#' @inheritParams chk_all_x1_in_x2
#' @param x1 A scalar or vector.
#' @param x2 A scalar or vector the same length as \code{x1},
#' or length 1.
#'
#' @seealso \code{\link{chk_ge}},  \code{\link{chk_le}},
#' \code{\link{chk_lt}}
#'
#' @examples
#' x1 <- 3.2
#' x2 <- 3.1
#' chk_gt_scalar(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' x1 <- c(3.2, 4.2, 5.7)
#' x2 <- c(3.1, 4.0, 4.2)
#' chk_gt_vector(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' x1 <- c(3.2, 4.2, 5.7)
#' x2 <- 3.1
#' chk_gt_vector(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' @name chk_gt
NULL

## HAS_TESTS
#' @export
#' @rdname chk_gt
chk_gt_scalar <- function(x1, x2, name1, name2) {
    if (!is.na(x1) && !is.na(x2) && !(x1 > x2))
        return(gettextf("'%s' [%s] is less than or equal to '%s' [%s]",
                        name1, x1, name2, x2))
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname chk_gt
chk_gt_vector <- function(x1, x2, name1, name2) {
    n1 <- length(x1)
    n2 <- length(x2)
    if ((n2 != n1) && (n2 != 1L))
        return(gettextf("'%s' has length %d and '%s' has length %d",
                        name1, n1, name2, n2))
    is_lt <- !is.na(x1) & !is.na(x2) & !(x1 > x2)
    i <- match(TRUE, is_lt, nomatch = 0L)    
    if (i > 0L) {
        if (n2 > 1L)
            return(gettextf("element %d of '%s' [%s] is less than or equal to element %d of '%s' [%s]",
                            i, name1, x1[[i]], i, name2, x2[[i]]))
        else
            return(gettextf("element %d of '%s' [%s] is less than or equal to '%s' [%s]",
                            i, name1, x1[[i]], name2, x2))            
    }
    TRUE
}


#' Check that 'x' is a non-NA logical vector of length 1
#'
#' @inheritParams chk_array_metadata_complete
#' @param x A scalar.
#'
#' @examples
#' x <- TRUE
#' chk_is_logical_flag(x, name = "x")
#' @export
chk_is_logical_flag <- function(x, name) {
    val <- chk_is_logical(x = x, name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_length_1(x = x, name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_not_na_scalar(x = x, name = name)
    if (!isTRUE(val))
        return(val)
    TRUE
}


#' Check that 'x1' is less than or equal to 'x2'
#'
#' @inheritParams chk_all_x1_in_x2
#' @param x1 A scalar or vector.
#' @param x2 A scalar or vector the same length as \code{x1},
#' or length 1.
#'
#' @seealso \code{\link{chk_lt}}, \code{\link{chk_ge}},
#' \code{\link{chk_gt}}
#'
#' @examples
#' x1 <- 2.2
#' x2 <- 3.1
#' chk_le_scalar(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' x1 <- c(3.3, 4.2, 5.7)
#' x2 <- c(3.3, 4.6, 6.2)
#' chk_le_vector(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' x1 <- c(3.3, 4.2, 5.7)
#' x2 <- 3.3
#' chk_le_vector(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' @name chk_le
NULL

## HAS_TESTS
#' @export
#' @rdname chk_le
chk_le_scalar <- function(x1, x2, name1, name2) {
    if (!is.na(x1) && !is.na(x2) && (x1 > x2))
        return(gettextf("'%s' [%s] is greater than '%s' [%s]",
                        name1, x1, name2, x2))
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname chk_le
chk_le_vector <- function(x1, x2, name1, name2) {
    n1 <- length(x1)
    n2 <- length(x2)
    if ((n2 != n1) && (n2 != 1L))
        return(gettextf("'%s' has length %d and '%s' has length %d",
                        name1, n1, name2, n2))
    is_ge <- !is.na(x1) & !is.na(x2) & (x1 > x2)
    i <- match(TRUE, is_ge, nomatch = 0L)    
    if (i > 0L) {
        if (n2 > 1L)
            return(gettextf("element %d of '%s' [%s] is greater than element %d of '%s' [%s]",
                            i, name1, x1[[i]], i, name2, x2[[i]]))
        else
            return(gettextf("element %d of '%s' [%s] is greater than '%s' [%s]",
                            i, name1, x1[[i]], name2, x2))
    }
    TRUE
}


#' Check that 'x1' is less than 'x2'
#'
#' @inheritParams chk_all_x1_in_x2
#' @param x1 A scalar or vector.
#' @param x2 A scalar or vector the same length as \code{x1},
#' or length 1.
#'
#' @seealso \code{\link{chk_le}}, \code{\link{chk_ge}},
#' \code{\link{chk_gt}}
#'
#' @examples
#' x1 <- 2.2
#' x2 <- 3.1
#' chk_lt_scalar(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' x1 <- c(3.2, 4.2, 5.7)
#' x2 <- c(3.3, 4.6, 6.2)
#' chk_lt_vector(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' x1 <- c(3.2, 4.2, 5.7)
#' x2 <- 3.3
#' chk_lt_vector(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' @name chk_lt
NULL

## HAS_TESTS
#' @export
#' @rdname chk_lt
chk_lt_scalar <- function(x1, x2, name1, name2) {
    if (!is.na(x1) && !is.na(x2) && !(x1 < x2))
        return(gettextf("'%s' [%s] is greater than or equal to '%s' [%s]",
                        name1, x1, name2, x2))
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname chk_lt
chk_lt_vector <- function(x1, x2, name1, name2) {
    n1 <- length(x1)
    n2 <- length(x2)
    if ((n2 != n1) && (n2 != 1L))
        return(gettextf("'%s' has length %d and '%s' has length %d",
                        name1, n1, name2, n2))
    is_ge <- !is.na(x1) & !is.na(x2) & !(x1 < x2)
    i <- match(TRUE, is_ge, nomatch = 0L)    
    if (i > 0L) {
        if (n2 > 1L)
            return(gettextf("element %d of '%s' [%s] is greater than or equal to element %d of '%s' [%s]",
                            i, name1, x1[[i]], i, name2, x2[[i]]))
        else
            return(gettextf("element %d of '%s' [%s] is greater than or equal to '%s' [%s]",
                            i, name1, x1[[i]], name2, x2))
    }
    TRUE
}


#' Check that 'x1' is a multiple of 'x2'
#'
#' If \code{null_ok} is \code{TRUE} and \code{x1}
#' is \code{NULL} then the test succeeds.
#' 
#' @inheritParams chk_all_x1_in_x2
#' @param x1 A scalar.
#' @param x2 A scalar.
#' @param null_ok Whether \code{x1} can be \code{NULL},
#' 
#' @seealso \code{\link{chk_multiple_of_n}}
#'
#' @examples
#' x1 <- 10L
#' x2 <- 2L
#' chk_multiple_of(x1 = x1, x2 = x2,
#'                 name1 = "x1", name2 = "x2",
#'                 null_ok = FALSE)
#' x1 <- NULL
#' chk_multiple_of(x1 = x1, x2 = x2,
#'                 name1 = "x1", name2 = "x2",
#'                 null_ok = TRUE)
#' @export
chk_multiple_of <- function(x1, x2, name1, name2, null_ok) {
    if (is.null(x1)) {
        if (!null_ok)
            return(gettextf("'%s' is %s",
                            name1, "NULL"))
    }
    else {
        if (x1 %% x2 != 0L)
            return(gettextf("'%s' [%s] is not a multiple of '%s' [%s]",
                          name1, x1, name2, x2))
    }
    TRUE
}


#' Check that 'x1' is a multiple of 'n'
#'
#' \code{chk_multiple_of_n} differs from
#' \code{\link{chk_multiple_of}} only in the
#' error message. \code{chk_multiple_of_n} refers
#' to the value of \code{n}, rather than to the
#' variable \code{"n"}.
#' 
#' @inheritParams chk_array_metadata_complete
#' @param x A scalar.
#' @param n A scalar.
#' @param null_ok Whether \code{x} can be \code{NULL},
#' 
#' @examples
#' x <- 10L
#' n <- 2L
#' chk_multiple_of_n(x = x, n = n, name = "x",
#'                   null_ok = FALSE)
#' x <- NULL
#' chk_multiple_of_n(x = x, n = n, name = "x",
#'                   null_ok = FALSE)
#' @export
chk_multiple_of_n <- function(x, name, n, null_ok) {
    if (is.null(x)) {
        if (!null_ok)
            return(gettextf("'%s' is %s",
                            name, "NULL"))
    }
    else {
        if (x %% n != 0L)
            return(gettextf("'%s' [%s] is not a multiple of %d",
                            name, x, n))
    }
    TRUE
}


#' Check that elements of a scalar or vector are non-negative
#'
#' @inheritParams chk_array_metadata_complete
#' @param x A scalar or vector.
#'
#' @seealso \code{\link{chk_positive_scalar}}
#'
#' @examples
#' x <- 0.1
#' chk_non_negative_scalar(x, name = "x")
#' x <- c(0.1, 0, 100)
#' chk_non_negative_vector(x, name = "x")
#' @name chk_non_negative
NULL

## HAS_TESTS
#' @export
#' @rdname chk_non_negative
chk_non_negative_scalar <- function(x, name) {
    val <- chk_length_1(x = x,
                           name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_not_na_scalar(x = x,
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
#' @rdname chk_non_negative
chk_non_negative_vector <- function(x, name) {
    val <- chk_not_na_vector(x = x,
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


#' Check that integers 'x1' and 'x2' are not equal
#'
#' @inheritParams chk_all_x1_in_x2
#' @param x1 An integer scalar.
#' @param x2 An integer scalar.
#'
#' @seealso \code{\link{chk_le}},
#' \code{\link{chk_lt}}, \code{\link{chk_ge}},
#' \code{\link{chk_gt}}
#'
#' @examples
#' x1 <- 2L
#' x2 <- 3L
#' chk_not_equal_integer_scalar(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' @export
chk_not_equal_integer_scalar <- function(x1, x2, name1, name2) {
    if (!is.na(x1) && !is.na(x2) && (x1 == x2))
        return(gettextf("'%s' [%d] is equal to '%s' [%d]",
                        name1, x1, name2, x2))
    TRUE
}


#' Check NULL status of 'x1' given NULL status of 'x2'
#'
#' \code{chk_null_if_null} checks that \code{x1} is \code{NULL},
#' given that \code{x2} is \code{NULL}.
#'
#' \code{chk_null_onlyif_null} checks that \code{x1} is only
#' \code{NULL} when \code{x2} is \code{NULL}.
#'
#' \code{chk_null_ifonlyif_null} checks that \code{x1} and
#' \code{x2} are both \code{NULL} or are both non-\code{NULL}.
#' 
#' @inheritParams chk_all_x1_in_x2
#' @param x1 An argument that could be \code{NULL}.
#' @param x2 An argument that could be \code{NULL}.
#'
#' @seealso \code{\link{chk_zero}}
#'
#' @examples
#' x1 <- NULL
#' x2 <- 2
#' chk_null_if_null(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' x1 <- NULL
#' x2 <- NULL
#' chk_null_if_null(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' x1 <- 1
#' x2 <- NULL
#' chk_null_onlyif_null(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' x1 <- NULL
#' x2 <- NULL
#' chk_null_onlyif_null(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' x1 <- 1
#' x2 <- 2
#' chk_null_ifonlyif_null(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' x1 <- NULL
#' x2 <- NULL
#' chk_null_ifonlyif_null(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' @name chk_null
NULL

#' @rdname chk_null
#' @export
chk_null_if_null <- function(x1, x2, name1, name2) {
    if (!is.null(x1) && is.null(x2))
        return(gettextf("'%s' is %s [%s] but '%s' is %s",
                        name1, "non-NULL", x1, name2, "NULL"))
    TRUE
}

#' @rdname chk_null
#' @export
chk_null_onlyif_null <- function(x1, x2, name1, name2) {
    if (is.null(x1) && !is.null(x2))
        return(gettextf("'%s' is %s but '%s' is %s [%s]",
                        name1, "NULL", name2, "non-NULL", x2))
    TRUE
}

#' @rdname chk_null
#' @export
chk_null_ifonlyif_null <- function(x1, x2, name1, name2) {
    val <- chk_null_if_null(x1 = x1,
                            x2 = x2,
                            name1 = name1,
                            name2 = name2)
    if (!isTRUE(val))
        return(val)
    val <- chk_null_onlyif_null(x1 = x1,
                                x2 = x2,
                                name1 = name1,
                                name2 = name2)
    if (!isTRUE(val))
        return(val)
    TRUE
}




#' Check that elements of a scalar or vector are positive
#'
#' @inheritParams chk_array_metadata_complete
#' @param x A scalar or vector.
#'
#' @seealso \code{\link{chk_non_negative_scalar}}
#'
#' @examples
#' x <- 0.1
#' chk_positive_scalar(x, name = "x")
#' x <- c(0.1, 0.0001, 100)
#' chk_positive_vector(x, name = "x")
#' @name chk_positive
NULL

## HAS_TESTS
#' @export
#' @rdname chk_positive
chk_positive_scalar <- function(x, name) {
    val <- chk_length_1(x = x,
                           name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_not_na_scalar(x = x,
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
#' @rdname chk_positive
chk_positive_vector <- function(x, name) {
    val <- chk_not_na_vector(x = x,
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
#' Check that an object is a non-NA character
#' vector of length 1
#'
#' @inheritParams chk_array_metadata_complete
#' @param x An object.
#'
#' @examples
#' x <- "hello"
#' chk_is_string(x, name = "x")
#' @export
chk_is_string <- function(x, name) {
    val <- chk_is_character(x = x, name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_length_1(x = x, name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_not_na_scalar(x = x, name = name)
    if (!isTRUE(val))
        return(val)
    TRUE
}


## HAS_TESTS
#' Check that elements in a vector of
#' numbers or dates are strictly increasing
#'
#' @inheritParams chk_array_metadata_complete
#' @param x A numeric vector
#'
#' @examples
#' x <- 1:5
#' chk_strictly_increasing(x, name = "x")
#' @export
chk_strictly_increasing <- function(x, name) {
    val <- chk_is_date_or_numeric(x = x, name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_not_na_vector(x = x, name = name)
    if (!isTRUE(val))
        return(val)
    if (length(x) >= 2L) {
        is_not_incr <- diff(x) <= 0L
        i_not_incr <- match(TRUE, is_not_incr, nomatch = 0L)
        if (i_not_incr > 0L) {
            return(gettextf("'%s' is not strictly increasing : element %d [%s] is greater than or equal to element %d [%s]",
                            name,
                            i_not_incr,
                            x[[i_not_incr]],
                            i_not_incr + 1L,
                            x[[i_not_incr + 1L]]))
        }
    }
    TRUE
}




#' Check that an array has dimnames
#'
#' Check that an array has dimnames, and that
#' those dimnames have names.
#'
#' @inheritParams chk_all_0_1
#' @param x An array.
#'
#' @seealso \code{\link{chk_no_dimnames}},
#' \code{\link{chk_array_metadata_complete}}
#' 
#' @examples
#' x <- array(1:6,
#'            dim = 3:2,
#'            dimnames = list(age = c("0-4", "5-9", "10+"),
#'                            sex = c("Female", "Male")))
#' chk_has_dimnames(x, name = "x")
#' chk_has_names_dimnames(x, name = "x")
#' @name chk_has_dimnames
NULL

#' @export
#' @rdname chk_has_dimnames
chk_has_dimnames <- function(x, name) {
    if (is.null(dimnames(x)))
        return(gettextf("'%s' does not have dimnames",
                        name))
    TRUE
}

#' @export
#' @rdname chk_has_dimnames
chk_has_names_dimnames <- function(x, name) {
    if (is.null(names(dimnames(x))))
        return(gettextf("dimnames for '%s' do not have names",
                        name))
    TRUE
}


#' Check that dimension indices all refer
#' to different dimensions
#'
#' @param indices A list of integer vectors
#' (typically of length 1).
#' @param names A character vector with the names
#' of the indices.
#' @param exclude_zero Logical. Whether to
#' exclude zeros in \code{indices} before
#' testing for uniqueness.
#'
#' @examples
#' indices <- list(3L, 1:2, 0L, 6:5)
#' names <- c("i_time", "i_orig", "i_age", "i_dest")
#' exclude_zero <- TRUE
#' chk_indices_distinct(indices = indices,
#'                      names = names,
#'                      exclude_zero = exclude_zero)
#' @export
chk_indices_distinct <- function(indices, names, exclude_zero) {
    indices_all <- unlist(indices)
    if (exclude_zero)
        indices_all <- indices_all[indices_all != 0L]
    has_duplicates <- any(duplicated(indices_all))
    if (has_duplicates) {
        names <- sprintf("'%s'", names)
        indices <- vapply(indices, paste, character(1L), collapse = ",")
        indices <- sprintf("[%s]", indices)
        names_indices <- paste(names, indices)
        names_indices <- paste(names, indices, collapse = ", ")
        return(gettextf("indices %s overlap",
                        names_indices))
    }
    TRUE
}




#' Check that 'x1' has length specified by 'x2'
#' 
#' @inheritParams chk_all_x1_in_x2
#' @param x1 An object.
#' @param x2 A positive integer.
#'
#' @seealso \code{\link{chk_length_same_or_1}},
#' \code{\link{chk_length_same}}
#'
#' @examples
#' chk_length_equals(x1 = c(1L, 3L, 2L),
#'                   x2 = 3L,
#'                   name1 = "x1",
#'                   name2 = "x2")
#'@export
chk_length_equals <- function(x1, x2, name1, name2) {
    n1 <- length(x1)
    if (!identical(n1, x2))
        return(gettextf("length of '%s' [%d] not equal to '%s' [%d]",
                        name1, n1, name2, x2))
    TRUE
}


#' Check that 'x1' and 'x2' have same length
#'
#' @inheritParams chk_all_x1_in_x2
#' @param x1 An object.
#' @param x2 An object.
#' 
#' @seealso \code{\link{chk_length_equals}},
#' \code{\link{chk_length_same_or_1}}
#'
#' @examples
#' x1 <- 1:5
#' x2 <- 2:6
#' chk_length_same(x1 = x1, x2 = x2,
#'                 name1 = "x1", name2 = "x2")
#' @export
chk_length_same <- function(x1, x2, name1, name2) {
    n1 <- length(x1)
    n2 <- length(x2)
    if (n1 != n2)
        return(gettextf("length of '%s' [%d] not equal to length of '%s' [%d]",
                        name1, n1, name2, n2))
    TRUE
}


#' Check that 'x1' and 'x2' have same
#' (non-zero) length or has length 1
#'
#' @inheritParams chk_all_x1_in_x2
#' @param x1 An object.
#' @param x2 An object.
#' 
#' @seealso \code{\link{chk_length_equals}},
# '\code{\link{chk_length_same}}
#'
#' @examples
#' x1 <- 1:5
#' x2 <- 2:6
#' chk_length_same_or_1(x1 = x1, x2 = x2,
#'                      name1 = "x1", name2 = "x2")
#' x2 <- 1L
#' chk_length_same_or_1(x1 = x1, x2 = x2,
#'                      name1 = "x1", name2 = "x2")
#' @export
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


## HAS TESTS
#' Check that 'x2' gives the lengths of the elements of 'x1'
#'
#' @inheritParams chk_all_x1_in_x2
#' @param x1 A list.
#' @param x2 An integer vector, the same length as \code{x1}.
#'
#' @examples
#' x1 <- list(1:4, 1:2, 1:3)
#' x2 <- c(4L, 2L, 3L)
#' chk_lengths_elements_equal_vec(x1 = x1, x2 = x2,
#'                                name1 = "x1", name2 = "x2")
#' @export
chk_lengths_elements_equal_vec <- function(x1, x2, name1, name2) {
    for (i in seq_along(x1)) {
        n1 <- length(x1[[i]])
        n2 <- x2[[i]]
        if (!identical(n1, n2)) {
            return(gettextf("length of element %d of '%s' [%d] not equal to element %d of '%s' [%d]",
                            i, name1, n1, i, name2, n2))
        }
    }
    TRUE
}    
        

## HAS_TESTS
#' Check that implied ages are less than 'break_max'
#'
#' @inheritParams chk_ge_break_min_age
#' @param break_max Integer or NULL. If non-NULL, upper limit
#' for \code{age}.
#' @param date Date on which event occurred or measurement made.
#' Object of class \code{\link[base:Dates]{Date}}.
#' @param dob Date of birth. Object of class \code{\link[base:Dates]{Date}}.
#' @param unit Measurement units for age, eg \code{"month"}.
#' 
#' @seealso \code{\link{chk_ge_break_min_age}},
#' \code{\link{chk_ge_break_min_date}},
#' \code{\link{chk_lt_break_max_date}}
#'
#' @examples
#' age <- c(80L, 83L)
#' break_max <- 85L
#' date <- as.Date(c("2020-03-17", "2020-03-18"))
#' dob <- as.Date(c("1940-02-01", "1937-01-13"))
#' unit <- "years"
#' chk_lt_break_max_age(age = age,
#'                      break_max = break_max,
#'                      date = date,
#'                      dob = dob,
#'                      unit = unit)
#' @export
chk_lt_break_max_age <- function(age, break_max, date, dob, unit) {
    if (is.null(break_max))
        return(TRUE)
    ge_max <- !is.na(age) & (age >= break_max)
    i <- match(TRUE, ge_max, nomatch = 0L)
    if (i > 0L) {
        return(gettextf(paste("'%s' [\"%s\"] and '%s' [\"%s\"] imply an age of %d %ss,",
                              "which is greater than or equal to '%s' [%d %ss]"),
                        "date",
                        date[[i]],
                        "dob",
                        dob[[i]],
                        age[[i]],
                        unit,
                        "break_max",
                        break_max,
                        unit))
    }
    TRUE
}


## HAS_TESTS
#' Check that dates are less than 'break_max'
#'
#' @inheritParams chk_ge_break_min_date
#' @param break_max Date or NULL. If non-NULL, upper limit
#' for \code{date}.
#' 
#' @seealso \code{\link{chk_ge_break_min_age}},
#' \code{\link{chk_ge_break_min_date}},
#' \code{\link{chk_lt_break_max_age}}
#'
#' @examples
#' date <- as.Date(c("2020-03-17", "2020-03-18"))
#' break_max <- as.Date("2020-06-01")
#' chk_lt_break_max_date(date = date, break_max = break_max)
#' @export
chk_lt_break_max_date <- function(date, break_max) {
    if (is.null(break_max))
        return(TRUE)
    ge_date_max <- !is.na(date) & (date >= break_max)
    i <- match(TRUE, ge_date_max, nomatch = 0L)
    if (i > 0L) {
        return(gettextf("'%s' has value [\"%s\"] that is greater than or equal to '%s' [\"%s\"]",
                        "date",
                        date[[i]],
                        "break_max",
                        break_max))
    }
    TRUE
}


## HAS_TESTS
#' Check that an object has a complete set of names
#'
#' @inheritParams chk_array_metadata_complete
#' @param x An object, typically a vector.
#'
#' @seealso \code{\link{chk_names_dimnames_complete}},
#' \code{\link{chk_has_dimnames}},
#' \code{\link{chk_array_metadata_complete}}
#'
#' @examples
#' x <- c(a = 1, b = 1.2)
#' chk_names_complete(x = x, name = "x")#' 
#' @export
chk_names_complete <- function(x, name) {
    nms <- names(x)
    if ((length(x) > 0L) && is.null(nms))
        return(gettextf("'%s' does not have names",
                        name))
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
#' Check that names for an
#' array are complete
#'
#' @inheritParams chk_array_metadata_complete
#' @param x An array.
#' 
#' @seealso \code{\link{chk_names_complete}},
#' \code{\link{chk_has_dimnames}},
#' \code{\link{chk_array_metadata_complete}}
#'
#' @examples
#' x <- array(1:6,
#'            dim = 2:3,
#'            dimnames = list(sex = c("Female", "Male"),
#'                            region = c("A", "B", "C")))
#' chk_names_dimnames_complete(x = x, name = "x")
#' @export
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


#' Check a 'dim' argument for an array with
#' positive length
#'
#' Check that \code{x} is a valid \code{dim}
#' argument for an array in which all dimensions
#' have length greater than 0.
#'
#' @inheritParams chk_array_metadata_complete
#' @param x An integer vector.
#'
#' @examples
#' chk_positive_dim(x = c(2L, 1L, 3L),
#'                  name = "x")
#' @export
chk_positive_dim <- function(x, name) {
    val <- demcheck::chk_is_integer(x = x,
                                    name = name)
    if (!isTRUE(val))
        return(val)
    val <- demcheck::chk_positive_length(x = x,
                                         name = name)
    if (!isTRUE(val))
        return(val)
    val <- demcheck::chk_positive_vector(x = x,
                                         name = name)
    if (!isTRUE(val))
        return(val)
    TRUE
}


## HAS_TESTS
#' Check that a vector is a valid quantile
#'
#' \code{chk_valid_quantile} checks that the elements
#' of \code{x} conform to R conventions about
#' formatting quantiles. \code{chk_quantile_increasing}
#' assumes that \code{x} is correctly formatted,
#' and checks that the elements are strictly increasing.
#'
#' @inheritParams chk_array_metadata_complete
#' @param x A character vector.
#'
#' @examples
#' x <- c("2.5%", "50%", "97.5")
#' chk_valid_quantile(x, name = "x")
#' chk_quantile_increasing(x, name = "x")
#' @name chk_valid_quantile
NULL

#' @name chk_valid_quantile
#' @export
chk_valid_quantile <- function(x, name) {
    p <- "^[0-9.]+%$" # excludes negative numbers
    is_na <- is.na(x)
    is_invalid <- !is_na & !grepl(p, x)
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L)
        return(gettextf("\"%s\" is not a valid quantile",
                        x[[i_invalid]]))
    x_num <- sub("%$", "", x)
    x_num <- suppressWarnings(as.numeric(x_num))
    is_invalid <- !is_na & is.na(x_num)
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L)
        return(gettextf("\"%s\" is not a valid quantile",
                        x[[i_invalid]]))
    is_gt_100 <- !is_na & (x_num > 100)
    i_gt_100 <- match(TRUE, is_gt_100, nomatch = 0L)
    if (i_gt_100 > 0L)
        return(gettextf("\"%s\" is not a valid quantile : greater than %s",
                        x[[i_gt_100]], "100%"))
    TRUE
}

#' @rdname chk_valid_quantile
#' @export
chk_quantile_increasing <- function(x, name) {
    p <- "%$"
    numbers <- sub(p, "", x)
    numbers <- as.numeric(numbers)
    if (length(x) >= 2L) {
        is_not_incr <- diff(numbers) <= 0L
        i_not_incr <- match(TRUE, is_not_incr, nomatch = 0L)
        if (i_not_incr > 0L) {
            return(gettextf(paste("'%s' is not strictly increasing : element %d [%s]",
                                  "is greater than or equal to element %d [%s]"),
                            name,
                            i_not_incr,
                            x[[i_not_incr]],
                            i_not_incr + 1L,
                            x[[i_not_incr + 1L]]))
        }
    }
    TRUE
}


#' Check if 'x1' is zero, conditional on whether 'x2' is zero
#'
#' \code{chk_zero_if_zero} checks that \code{x1} is \code{0},
#' given that \code{x2} is \code{0}.
#'
#' \code{chk_zero_onlyif_zero} checks that \code{x1} is only
#' \code{0} when \code{x2} is \code{0}.
#'
#' \code{chk_zero_ifonlyif_zero} checks that \code{x1} and
#' \code{x2} are both \code{0} or are both non-\code{0}.
#' 
#' @inheritParams chk_all_x1_in_x2
#' @param x1 An argument that could be \code{0}.
#' @param x2 An argument that could be \code{0}.
#'
#' @seealso \code{\link{chk_zero}}
#'
#' @examples
#' x1 <- 0
#' x2 <- 2
#' chk_zero_if_zero(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' x1 <- 0
#' x2 <- 0
#' chk_zero_if_zero(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' x1 <- 1
#' x2 <- 0
#' chk_zero_onlyif_zero(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' x1 <- 0
#' x2 <- 0
#' chk_zero_onlyif_zero(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' x1 <- 1
#' x2 <- 2
#' chk_zero_ifonlyif_zero(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' x1 <- 0
#' x2 <- 0
#' chk_zero_ifonlyif_zero(x1 = x1, x2 = x2, name1 = "x1", name2 = "x2")
#' @name chk_zero
NULL

#' @rdname chk_zero
#' @export
chk_zero_if_zero <- function(x1, x2, name1, name2) {
    if (!isTRUE(all.equal(x1, 0L)) && isTRUE(all.equal(x2, 0L)))
        return(gettextf("'%s' [%s] does not equal %d but '%s' equals %s",
                        name1, x1, 0L, name2, 0L))
    TRUE
}

#' @rdname chk_zero
#' @export
chk_zero_onlyif_zero <- function(x1, x2, name1, name2) {
    if (isTRUE(all.equal(x1, 0L)) && !isTRUE(all.equal(x2, 0L)))
        return(gettextf("'%s' equals %d but '%s' [%s] does not equal %d",
                        name1, 0L, name2, x2, 0L))
    TRUE
}

#' @rdname chk_zero
#' @export
chk_zero_ifonlyif_zero <- function(x1, x2, name1, name2) {
    val <- chk_zero_if_zero(x1 = x1,
                            x2 = x2,
                            name1 = name1,
                            name2 = name2)
    if (!isTRUE(val))
        return(val)
    val <- chk_zero_onlyif_zero(x1 = x1,
                                x2 = x2,
                                name1 = name1,
                                name2 = name2)
    if (!isTRUE(val))
        return(val)
    TRUE
}

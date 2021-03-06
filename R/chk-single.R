
## Checks of single attributes. No sanity checking of
## arguments is carried out, on the assumption that
## these functions will generally be called from within
## other, more complicated, checking functions.

#' Check that a vector consists entirely of of 1s and 0s
#'
#' @param x An integer vector.
#' @param name The name for \code{x} that
#' will be used in error messages.
#'
#' @examples
#' x <- c(0L, 1L, 0L, 0L, 1L)
#' chk_all_0_1(x, name = "x")
#' @export
chk_all_0_1 <- function(x, name) {
    if (!all(x %in% 0:1))
        return(gettextf("'%s' has values other than %d or %d'",
                        name, 0L, 1L))
    TRUE
}

## HAS_TESTS
#' Check that a vector or list has 0 or 1 NAs
#'
#' In the case of a list, an item is 'NA'
#' if it has at least one element,
#' and of these elements are NA
#'
#' @inheritParams chk_all_0_1
#'
#' @examples
#' x <- c(0L, 1L, NA)
#' chk_at_most_one_na_vector(x, name = "x")
#' x <- c(c(0L, 1L), c(2L, NA))
#' chk_at_most_one_na_list(x, name = "x")
#' @name chk_at_most_one_na_vector
NULL

#' @rdname chk_at_most_one_na_vector
#' @export
chk_at_most_one_na_vector <- function(x, name) {
    n_na <- sum(is.na(x))
    if (n_na > 1L)
        return(gettextf("'%s' has %d NAs",
                        name, n_na))
    TRUE
}

#' @rdname chk_at_most_one_na_vector
#' @export
chk_at_most_one_na_list <- function(x, name) {
    is_positive_length <- sapply(x, length) > 0L
    is_all_na <- sapply(lapply(x, is.na), all)
    n_na <- sum(is_positive_length & is_all_na)
    if (n_na > 1L)
        return(gettextf("'%s' has %d items where all elements are NAs",
                        name, n_na))
    TRUE
}



#' Check that a value has type "character"
#'
#' @inheritParams chk_all_0_1
#' @param x A vector.
#'
#' @examples
#' x <- c("a", "b", "c")
#' chk_is_character(x, name = "x")
#' @export
chk_is_character <- function(x, name) {
    if (!is.character(x))
        return(gettextf("'%s' does not have type \"%s\"",
                        name, "character"))
    TRUE
}


#' Check that a value inherits from class "Date"
#'
#' @inheritParams chk_all_0_1
#' @param x A vector.
#'
#' @examples
#' x <- as.Date(c("2020-01-01", "2021-01-01"))
#' chk_is_date(x, name = "x")
#' @export
chk_is_date <- function(x, name) {
    if (!inherits(x, "Date"))
        return(gettextf("'%s' does not have class \"%s\"",
                        name, "Date"))
    TRUE
}


#' Check that a scalar or vector is dates or
#' can be coerced to dates
#' 
#' @inheritParams chk_all_0_1
#' @param x A scalar or a vector.
#'
#' @seealso \code{\link{chk_is_date}} for a stricter test,
#' and \code{\link{err_tdy_date_scalar}} and
#' \code{\link{err_tdy_date_vector}} to coerce
#' where necessary.
#'
#' @examples
#' x <- as.Date("2001-12-31")
#' chk_is_date_equiv_scalar(x, name = "x")
#' x <- as.Date(c("2001-12-31", "2002-01-01"))
#' chk_is_date_equiv_vector(x, name = "x")
#' @name chk_is_date_equiv
NULL

## HAS_TESTS
#' @export
#' @rdname chk_is_date_equiv
chk_is_date_equiv_scalar <- function(x, name) {
    val <- tryCatch(error = function(e) e,
                    err_tdy_date_scalar(x = x,
                                        name = name))
    if (inherits(val, "error"))
        val$message
    else
        TRUE
}

## HAS_TESTS
#' @export
#' @rdname chk_is_date_equiv
chk_is_date_equiv_vector <- function(x, name) {
    val <- tryCatch(error = function(e) e,
                    err_tdy_date_vector(x = x,
                                        name = name))
    if (inherits(val, "error"))
        val$message
    else
        TRUE
}


#' Check that a vector is dates or numbers
#'
#' The vector can have length 1,
#' ie be a scalar.
#' 
#' @inheritParams chk_all_0_1
#' @param x A scalar or a vector.
#'
#' @examples
#' x <- as.Date("2001-03-01")
#' chk_is_date_or_numeric(x, name = "x")
#' x <- 5:1
#' chk_is_date_or_numeric(x, name = "x")
#' @export
chk_is_date_or_numeric <- function(x, name) {
    if (!inherits(x, "Date") && !is.numeric(x))
        return(gettextf("'%s' does not have class \"%s\" or \"%s\"",
                        name, "Date", "numeric"))
    TRUE
}


#' Check that a value has type "factor"
#'
#' @inheritParams chk_all_0_1
#' @param x A vector.
#'
#' @examples
#' x <- factor(c("a", "b", "c"))
#' chk_is_factor(x, name = "x")
#' @export
chk_is_factor <- function(x, name) {
    if (!is.factor(x))
        return(gettextf("'%s' does not have type \"%s\"",
                        name, "factor"))
    TRUE
}


#' Check that a scalar is NA, or a vector
#' is all NA
#'
#' @inheritParams chk_all_0_1
#' @param x A scalar or a vector.
#'
#' @seealso \code{\link{chk_not_na}}
#'
#' @examples
#' x <- NA
#' chk_is_na_scalar(x, name = "x")
#' x <- c(NA_integer_, NA_integer_)
#' chk_is_na_vector(x, name = "x")
#' @name chk_is_na
NULL

#' @rdname chk_is_na
#' @export
chk_is_na_scalar <- function(x, name) {
    if (!is.na(x))
        return(gettextf("'%s' is not %s",
                        "name", "NA"))
    TRUE
}

#' @rdname chk_is_na
#' @export
chk_is_na_vector <- function(x, name) {
    is_not_na <- !is.na(x)
    i_not_na <- match(TRUE, is_not_na, nomatch = 0L)
    if (i_not_na > 0L)
        return(gettextf("element %d of '%s' [%s] is not %s",
                        i_not_na, "name", x[[i_not_na]], "NA"))
    TRUE
}


#' Check that all items in a list are Date vectors
#'
#' @inheritParams chk_all_0_1
#' @param x A list.
#'
#' @examples
#' x <- list(as.Date(character()),
#'           as.Date(c("2000-01-01", "2001-02-01")))
#' chk_items_date(x = x,
#'                name = "x")
#' @export
chk_items_date <- function(x, name) {
    for (i in seq_along(x)) {
        item <- x[[i]]
        if (!inherits(item, "Date")) {
            return(gettextf("item %d of '%s' has class \"%s\"",
                            i, name, class(item)))
        }
    }
    TRUE
}


## HAS_TESTS
#' Check that elements within each item
#' are increasing or strictly increasing
#'
#' NAs are ignored.
#'
#' @inheritParams chk_all_0_1
#' @param x A list.
#' @param strict Logical. Whether elements should be
#' strictly increasing
#'
#' @examples
#' x <- list(c(0L, c(1L, 1L, 2L), 3:1, c(NA, 5, 10)))
#' chk_items_increasing(x = x,
#'                      strict = FALSE,
#'                      name = "x")
#' x <- list(c(0L, c(1L, NA, 2L), 3:1, c(NA, 5, 10)))
#' chk_items_increasing(x = x,
#'                      strict = TRUE,
#'                      name = "x")
#' @export
chk_items_increasing <- function(x, strict, name) {
    for (i in seq_along(x)) {
        item <- x[[i]]
        item <- item[!is.na(item)]
        if (length(item) > 1L) {
            diff <- diff(item)
            if (strict) {
                if (any(diff <= 0L)) {
                    val <- gettextf("elements of item %d of '%s' not strictly increasing",
                                    i, name)
                    return(val)
                }
            }
            else {
                if (any(diff < 0L)) {
                    val <- gettextf("elements of item %d of '%s' not increasing",
                                    i, name)
                    return(val)
                }
            }
        }
    }
    TRUE
}


#' Check that all items in a list are integer vectors
#'
#' @inheritParams chk_all_0_1
#' @param x A list.
#'
#' @examples
#' x <- list(integer(), 1:3, 0L)
#' chk_items_integer(x = x,
#'                   name = "x")
#' @export
chk_items_integer <- function(x, name) {
    for (i in seq_along(x)) {
        item <- x[[i]]
        if (!is.integer(item)) {
            val <- gettextf("item %d of '%s' has class \"%s\"",
                            i, name, class(item))
            return(val)
        }
    }
    TRUE
}


#' Check that all items in a list have length k
#'
#' @inheritParams chk_all_0_1
#' @param x A list.
#' @param k The required length of each item.
#'
#' @examples
#' x <- list(3:1, 1:3, c("a", "b", "c"))
#' chk_items_length_k(x = x,
#'                    k = 3L,
#'                    name = "x")
#' @export
chk_items_length_k <- function(x, k, name) {
    for (i in seq_along(x)) {
        item <- x[[i]]
        length <- length(item)
        if (!identical(length, k)) {
            val <- gettextf("item %d of '%s' has length %d",
                            i, name, length)
            return(val)
        }
    }
    TRUE
}


#' Check that not items have NAs, except where
#' specified in 'except' argument
#'
#' @inheritParams chk_all_0_1
#' @param x A list.
#' @param except A list of integer vectors, each of which is length 2.
#'
#' @examples
#' x <- list(c(0L, NA, 1L), 3:1, c(NA, "b", "c"), 1L)
#' chk_items_no_na(x = x,
#'                 except = list(c(1L, 2L), c(3L, 1L)),
#'                 name = "x")
#' @export
chk_items_no_na <- function(x, except, name) {
    i_item_na_allowed <- sapply(except, `[[`, 1L)
    i_element_na_allowed <- sapply(except, `[[`, 2L)
    for (i in seq_along(x)) {
        item <- x[[i]]
        item_has_exclusion <- i_item_na_allowed == i
        if (any(item_has_exclusion)) {
            i_exclude <- i_element_na_allowed[item_has_exclusion]
            item <- item[-i_exclude]
        }
        if (anyNA(item)) {
            val <- gettextf("item %d of '%s' has NA",
                            i, name)
            return(val)
        }
    }
    TRUE
}


## HAS_TESTS
#' Check that, in items of list, second element
#' one greater than first
#'
#' NAs are ignored.
#'
#' @inheritParams chk_all_0_1
#' @param x A list.
#'
#' @examples
#' x <- list(c(0L, 1L), c(1L, 2L), c(NA, 100L), c(NA, 5L))
#' chk_items_one_greater(x = x,
#'                       name = "x")
#' @export
chk_items_one_greater <- function(x, name) {
    for (i in seq_along(x)) {
        item <- x[[i]]
        if (!anyNA(item)) {
            el1 <- item[[1L]]
            el2 <- item[[2L]]
            if (!identical(el2 - el1, 1L)) {
                return(gettextf("second element [%d] of item %d is not one greater than first element [%d]",
                                el2, i, el1))
            }
        }
    }
    TRUE
}


#' Check that a scalar or vector only has finite values
#'
#' @inheritParams chk_all_0_1
#' @param x A numeric scalar.
#'
#' @examples
#' x <- 0
#' chk_finite_scalar(x, name = "x")
#' x <- 1:5
#' chk_finite_vector(x, name = "x")
#' @name chk_finite_scalar
NULL

#' @export
#' @rdname chk_finite_scalar
chk_finite_scalar <- function(x, name) {
    if (is.infinite(x))
        return(gettextf("'%s' is infinite",
                        name))
    TRUE
}

#' @export
#' @rdname chk_finite_scalar
chk_finite_vector <- function(x, name) {
    if (any(is.infinite(x)))
        return(gettextf("'%s' has infinite values",
                        name))
    TRUE
}


#' Check that a vector has at least one
#' non-NA element
#'
#' @inheritParams chk_all_0_1
#' @param x A scalar or a vector.
#'
#' @seealso \code{\link{chk_has_nonzero}}
#'
#' @examples
#' x <- c(0L, NA, NA)
#' chk_has_non_na(x = x, name = "x")
#' @export
chk_has_non_na <- function(x, name) {
    if (!any(!is.na(x)))
        return(gettextf("'%s' has no non-NA elements",
                        name))
    TRUE
}

#' Check that a vector has at least one
#' non-zero element
#'
#' @inheritParams chk_all_0_1
#' @param x A scalar or a vector.
#'
#' @seealso \code{\link{chk_has_non_na}}
#'
#' @examples
#' x <- c(0L, 1L, 2L, 0L)
#' chk_has_nonzero(x = x, name = "x")
#' @export
chk_has_nonzero <- function(x, name) {
    if (!any(x != 0L))
        return(gettextf("'%s' has no non-zero elements",
                        name))
    TRUE
}


#' Check that a scalar or vector has type integer
#'
#' Checking type, e.g. fails for 1.0.
#' \code{chk_is_integer_conse} adds the condition
#' that the integers be consecutive.
#' 
#' 
#' @inheritParams chk_all_0_1
#' @param x A scalar or a vector.
#'
#' @seealso \code{\link{chk_is_integer_equiv_scalar}} for looser
#' integer test. \code{\link{chk_is_numeric}} to test for
#' numeric type.
#'
#' @examples
#' x <- 1:3
#' chk_is_integer(x, name = "x")
#' chk_is_integer_consec(x, name = "x")
#' @name chk_is_integer
NULL

#' @rdname chk_is_integer
#' @export
chk_is_integer <- function(x, name) {
    if (!is.integer(x))
        return(gettextf("'%s' does not have type \"%s\"",
                        name, "integer"))
    TRUE
}

## HAS_TESTS
#' @export
#' @rdname chk_is_integer
chk_is_integer_consec <- function(x, name) {
    val <- chk_is_integer(x = x, name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_not_na_vector(x = x, name = name)
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


#' Check that a scalar or vector is integer or can be coerced to integer
#' 
#' @inheritParams chk_all_0_1
#' @param x A scalar or a vector.
#'
#' @seealso \code{\link{chk_is_integer}} for a stricter test,
#' and \code{\link{err_tdy_integer_scalar}} and
#' \code{\link{err_tdy_integer_vector}} to coerce
#' to integer where necessary.
#'
#' @examples
#' x <- 1.0
#' chk_is_integer_equiv_scalar(x, name = "x")
#' x <- 1:4
#' chk_is_integer_equiv_vector(x, name = "x")
#' @name chk_is_integer_equiv
NULL

#' @export
#' @rdname chk_is_integer_equiv
chk_is_integer_equiv_scalar <- function(x, name) {
    val <- tryCatch(error = function(e) e,
                    err_tdy_integer_scalar(x = x,
                                           name = name))
    if (inherits(val, "error"))
        val$message
    else
        TRUE
}

## HAS_TESTS
#' @export
#' @rdname chk_is_integer_equiv
chk_is_integer_equiv_vector <- function(x, name) {
    val <- tryCatch(error = function(e) e,
                    err_tdy_integer_vector(x = x,
                                           name = name))
    if (inherits(val, "error"))
        val$message
    else
        TRUE
}


#' Check that a scalar or vector has type numeric
#'
#' Checking type, e.g. fails for 1L.
#' 
#' @inheritParams chk_all_0_1
#' @param x A scalar or a vector.
#'
#' @seealso \code{\link{chk_is_integer}} to test for
#' integer type.
#'
#' @examples
#' x <- 1.0
#' chk_is_numeric(x, name = "x")
#' @export
chk_is_numeric <- function(x, name) {
    if (!is.numeric(x))
        return(gettextf("'%s' does not have type \"%s\"",
                        name, "numeric"))
    TRUE
}


#' Check that a vector has length 1,
#' ie is a scalar
#' 
#' @inheritParams chk_all_0_1
#' @param x A vector.
#'
#' @examples
#' x <- "a"
#' chk_length_1(x, name = "x")
#' @export
chk_length_1 <- function(x, name) {
    if (!identical(length(x), 1L))
        return(gettextf("'%s' does not have length %d",
                        name, 1L))
    TRUE
}


#' Check that a vector has class "list"
#' 
#' @inheritParams chk_all_0_1
#' @param x A vector.
#'
#' @examples
#' x <- list(1L, "a", NA)
#' chk_is_list(x, name = "x")
#' @export
chk_is_list <- function(x, name) {
    if (!is.list(x))
        return(gettextf("'%s' does not have type \"%s\"",
                        name, "list"))
    TRUE
}


#' Check that a vector has type "logical"
#' 
#' @inheritParams chk_all_0_1
#' @param x A vector.
#'
#' @examples
#' x <- c(TRUE, FALSE, NA)
#' chk_is_logical(x, name = "x")
#' @export
chk_is_logical <- function(x, name) {
    if (!is.logical(x))
        return(gettextf("'%s' does not have type \"%s\"",
                        name, "logical"))
    TRUE
}


#' Check that a vector has no duplicates
#'
#' @inheritParams chk_all_0_1
#' @param x A character scalar or vector.
#'
#' @examples
#' x <- c(0L, 5L, 10L, NA)
#' chk_no_duplicates(x, name = "x")
#' @export
chk_no_duplicates <- function(x, name) {
    is_duplicate <- duplicated(x)
    i_duplicate <- match(TRUE, is_duplicate, nomatch = 0L)
    if (i_duplicate > 0L) {
        if (is.character(x))
            fmt <- "element %d of '%s' [\"%s\"] is duplicate"
        else
            fmt <- "element %d of '%s' [%s] is duplicate"
        return(gettextf(fmt, i_duplicate, name, x[[i_duplicate]]))
    }
    TRUE
}

#' Check that calendar labels not open on left
#'
#' @inheritParams chk_all_0_1
#' @param x A character vector of calender interval labels.
#' @param unit The time unit: \code{"year"}, \code{"quarter"}
#' or \code{"month"}.
#'
#' @examples
#' chk_no_open_first(x = c("2000 Jan", "2005 Aug", "2006 Nov"),
#'                      name = "x",
#'                      unit = "month")
#' chk_no_open_first(c("2000 Q1", "2005 Q3", "2006 Q2"),
#'                      name = "x",
#'                      unit = "quarter")
#' chk_no_open_first(c("2000", "2005", "2006"),
#'                      name = "x",
#'                      unit = "year")
#' @export
chk_no_open_first <- function(x, name, unit) {
    if (unit == "month")
        p_open <- paste(sprintf("^<[0-9]+ %s$", base::month.abb), collapse = "|")
    else if (unit == "quarter")
        p_open <- "^<[0-9]+ Q[1-4]$"
    else if (unit == "year")
        p_open <- "^<[0-9]+$"
    else
        stop(gettextf("cannot handle unit \"%s\"",
                      unit))
    is_open <- grepl(p_open, x)
    i_open <- match(TRUE, is_open, nomatch = 0L)
    if (i_open > 0L)
        return(gettextf("'%s' has open interval [\"%s\"]",
                        name, x[[i_open]]))
    TRUE
}


#' Check that a character scalar or vector is not blank
#'
#' An element is blank if it equals "".
#' 
#' @inheritParams chk_all_0_1
#' @param x A character scalar or vector.
#'
#' @examples
#' x <- "A"
#' chk_not_blank_scalar(x, name = "x")
#' x <- c("A", "B", "C")
#' chk_not_blank_vector(x, name = "x")
#' @name chk_not_blank
NULL

#' @rdname chk_not_blank
#' @export
chk_not_blank_scalar <- function(x, name) {
    if (!nzchar(x))
        return(gettextf("'%s' is blank",
                        name))
    TRUE
}

#' @rdname chk_not_blank
#' @export
chk_not_blank_vector <- function(x, name) {
    if (any(!nzchar(x)))
        return(gettextf("'%s' has blanks",
                        name))
    TRUE
}


#' Check that an object does not have NAs
#'
#' @inheritParams chk_all_0_1
#' @param x A data frame, list,
#' scalar, or vector.
#'
#' @examples
#' x <- data.frame(a = 1:2, b = 3:4)
#' chk_not_na_dataframe(x, name = "x")
#' x <- list(a = 1:2, b = 3:4)
#' chk_not_na_list(x, name = "x")
#' x <- 1.3
#' chk_not_na_scalar(x, name = "x")
#' x <- 4:1
#' chk_not_na_vector(x, name = "x")
#' @name chk_not_na
NULL

#' @export
#' @rdname chk_not_na
chk_not_na_dataframe <- function(x, name) {
    for (i in seq_along(x)) {
        element_i <- x[[i]]
        if (anyNA(element_i))
            return(gettextf("column %d of '%s' has NAs",
                            i, name))
    }                                
    TRUE
}

#' @export
#' @rdname chk_not_na
chk_not_na_list <- function(x, name) {
    for (i in seq_along(x)) {
        element_i <- x[[i]]
        if (anyNA(element_i))
            return(gettextf("element %d of '%s' has NAs",
                            i, name))
    }                                
    TRUE
}

#' @export
#' @rdname chk_not_na
chk_not_na_scalar <- function(x, name) {
    if (is.na(x))
        return(gettextf("'%s' is NA",
                        name))
    TRUE
}

#' @export
#' @rdname chk_not_na
chk_not_na_vector <- function(x, name) {
    if (anyNA(x))
        return(gettextf("'%s' has NAs",
                        name))
    TRUE
}


#' Check that a vector has positive length,
#' ie not length 0
#'
#' @inheritParams chk_all_0_1
#' @param x A vector.
#'
#' @examples
#' x <- 1.0
#' chk_positive_length(x, name = "x")
#' @export
chk_positive_length <- function(x, name) {
    if (identical(length(x), 0L))
        return(gettextf("'%s' has length %d",
                        name, 0L))
    TRUE
}


#' Check that an array does not have dimnames
#'
#' @inheritParams chk_all_0_1
#' @param x A vector.
#'
#' @seealso \code{\link{chk_has_dimnames}} to test
#' that an array does have dimnames. \code{\link{chk_no_names}}
#' to test that an object does not have names.
#'
#' @examples
#' x <- array(1:6, dim = 3:2)
#' chk_no_dimnames(x, name = "x")
#' @export
chk_no_dimnames <- function(x, name) {
    if (!is.null(dimnames(x)))
        return(gettextf("'%s' has dimnames",
                        name))
    TRUE
}


#' Check that an object does not have names
#'
#' @inheritParams chk_all_0_1
#' @param x An object.
#'
#' @seealso \code{\link{chk_no_dimnames}} to test
#' that an array does not have dimnames.
#'
#' @examples
#' x <- 1:3
#' chk_no_names(x, name = "x")
#' @export
chk_no_names <- function(x, name) {
    if (!is.null(names(x)))
        return(gettextf("'%s' has names",
                        name))
    TRUE
}


## HAS_TESTS
#' Check that non-zero elements in an integer
#' vector are unique
#'
#' @inheritParams chk_all_0_1
#' @param x An object.
#'
#' @examples
#' x <- c(0L, 2L, 0L, 1L, 0L)
#' chk_nonzero_unique(x, name = "x")
#' 
#' @export
chk_nonzero_unique <- function(x, name) {
    x_nonzero <- x[x!= 0L]
    is_dup <- duplicated(x_nonzero)
    i_dup <- match(TRUE, is_dup, nomatch = 0L)
    if (i_dup > 0L)
        return(gettextf("non-zero element of '%s' is duplicated : %d",
                        name, x_nonzero[i_dup]))
    TRUE
}




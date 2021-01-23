
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




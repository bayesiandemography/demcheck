
## Checks of membership
##
## Functions to check whether a string belongs to a
## set of values.  Includes a check of
## whether \code{x} is a valid string, to provide
## more helpful error messages to users.

#' Check that 'x' is a valid type of account
#' component
#'
#' @param x A string.
#' @param name The name for \code{x} that
#' will be used in error messages.
#'
#' @examples
#' x <- "orig-dest"
#' chk_member_comp_type(x, name = "x")
#' @export
chk_member_comp_type <- function(x, name) {
    valid_members <- c("increment",
                       "decrement",
                       "orig-dest",
                       "pool")
    val <- chk_is_string(x = x,
                         name = name)
    if (!isTRUE(val))
        return(val)
    i <- match(x, valid_members, nomatch = 0L)
    if (i == 0L)
        return(gettextf("\"%s\" is not a valid component type",
                        x))
    TRUE
}


## HAS_TESTS
#' Check that 'x' consists of valid dimtypes
#'
#' @inheritParams chk_member_comp_type
#' @param x A character vector
#'
#' @examples
#' x <- c("age", "attribute")
#' chk_member_dimtype(x, name = "x")
#' @export
chk_member_dimtype <- function(x, name) {
    valid_members <- c("attribute",
                       "origin",
                       "destination",
                       "parent",
                       "child",
                       "age",
                       "time",
                       "cohort",
                       "triangle",
                       "pool",
                       "iteration",
                       "quantile")
    val <- chk_is_character(x = x,
                            name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_not_na_vector(x = x,
                             name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_not_blank_vector(x = x,
                                name = name)
    if (!isTRUE(val))
        return(val)
    is_invalid <- !(x %in% valid_members)
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L)
        return(gettextf("\"%s\" is not a valid dimtype",
                        x[[i_invalid]]))
    TRUE
}

## HAS_TESTS
#' Check that 'x' is a valid time unit
#'
#' @inheritParams chk_member_dimtype
#' @param x A string - ie a character vector of length 1.
#'
#' @examples
#' x <- "year"
#' chk_member_unit(x = x, name = "x")
#' @export
chk_member_unit <- function(x, name) {
    valid_members <- c("month",
                       "quarter",
                       "year")
    val <- chk_is_string(x = x,
                         name = name)
    if (!isTRUE(val))
        return(val)
    i <- match(x, valid_members, nomatch = 0L)
    if (i == 0L)
        return(gettextf("value for '%s' [\"%s\"] is not a valid time unit",
                        name, x))
    TRUE
}


        

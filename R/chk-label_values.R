
## Checks of the 'values' slot of "Labels" objects,
## or of the 'x' argument of 'make_labels' functions

## Categories -----------------------------------------------------------------

## NO_TESTS
#' Check values for Categories labels
#'
#' Checks:
#' \itemize{
#'   \item is character
#'   \item at most one NA
#'   \item no blanks (zero-length strings)
#'   \item no duplicates
#' }
#' 
#' @param x A character vector.
#' @param name The name for \code{x} that
#' will be used in error messages.
#'
#' @examples
#' x <- c("Thailand", "Laos", "Cambodia")
#' chk_label_values_categories(x = x, name = "x")
#' @export
chk_label_values_categories <- function(x, name) {
    ## is character
    val <- chk_is_character(x = x,
                            name = name)
    if (!isTRUE(val))
        return(val)
    ## at most one NA
    val <- chk_at_most_one_na_vector(x = x,
                                     name = name)
    if (!isTRUE(val))
        return(val)
    ## no blanks, or duplicates
    val <- chk_character_complete(x = x[!is.na(x)],
                                  name = name)
    if (!isTRUE(val))
        return(val)
    TRUE
}


## Specialised classes --------------------------------------------------------

## NO_TESTS
#' Check values for Triangles labels
#'
#' Checks:
#' \itemize{
#'   \item is character
#'   \item has values \code{c("Lower", "Upper")}, not necessarily in that order
#' }
#' 
#' @inheritParams chk_label_values_categories
#'
#' @examples
#' x <- c("Upper", "Lower")
#' chk_label_values_triangles(x = x, name = "x")
#' @export
chk_label_values_triangles <- function(x, name) {
    ## is character
    val <- chk_is_character(x = x,
                            name = name)
    if (!isTRUE(val))
        return(val)
    ## at most one NA
    val <- chk_at_most_one_na_vector(x = x,
                                     name = name)
    if (!isTRUE(val))
        return(val)
    ## has expected values (not necessarily in same order)
    if (!identical(sort(x[!is.na(x)]), c("Lower", "Upper"))) {
        x <- sprintf("\"%s\"", x)
        x <- paste(x, collapse = ", ")
        return(gettextf("'%s' [%s] not \"%s\", \"%s\"",
                        "x", x, "Lower", "Upper"))
    }
    TRUE
}

## NO_TESTS
#' Check values for Direction labels
#'
#' Checks:
#' \itemize{
#'   \item is character
#'   \item has values \code{c("In", "Out")}, not necessarily in that order
#' }
#' 
#' @inheritParams chk_label_values_categories
#'
#' @examples
#' x <- c("In", "Out")
#' chk_label_values_direction(x = x, name = "x")
#' @export
chk_label_values_direction <- function(x, name) {
    val <- chk_is_character(x = x,
                            name = name)
    if (!isTRUE(val))
        return(val)
    if (!identical(sort(x), c("In", "Out"))) {
        x <- sprintf("\"%s\"", x)
        x <- paste(x, collapse = ", ")
        return(gettextf("'%s' [%s] not \"%s\", \"%s\"",
                        "x", x, "In", "Out"))
    }
    TRUE
}

## NO_TESTS
#' Check values for Quantiles labels
#'
#' Checks:
#' \itemize{
#'   \item is character
#'   \item no NAs
#'   \item all elements have format "<number>%" where 0 <= number <= 100
#' }
#' 
#' @inheritParams chk_label_values_categories
#'
#' @examples
#' x <- c("2.5%", "50%", "97.5%")
#' chk_label_values_quantiles(x = x, name = "x")
#' @export
chk_label_values_quantiles <- function(x, name) {
    ## is character
    val <- chk_is_character(x = x,
                            name = name)
    if (!isTRUE(val))
        return(val)
    ## no NAs
    val <- chk_not_na_vector(x = x,
                             name = name)
    if (!isTRUE(val))
        return(val)
    ## has format of quantile
    val <- chk_valid_quantile(x = x,
                              name = name)
    if (!isTRUE(val))
        return(val)
    TRUE
}


## Numeric --------------------------------------------------------------------

## NO_TESTS
#' Check values for Integers labels
#'
#' Checks:
#' \itemize{
#'   \item is integer
#'   \item no duplicates
#'   \item no NAs
#' }
#'
#' @inheritParams chk_label_values_categories
#' @param x An integer vector.
#'
#' @examples
#' x <- c(0L, 5L, 22L, 23L)
#' chk_label_values_integers(x = x, name = "x")
#' @export
chk_label_values_integers <- function(x, name) {
    ## is integer
    val <- chk_is_integer(x = x,
                          name = name)
    if (!isTRUE(val))
        return(val)
    ## no NAs
    val <- chk_not_na_vector(x = x,
                             name = name)
    if (!isTRUE(val))
        return(val)
    ## no duplicates
    val <- chk_no_duplicates(x = x,
                             name = name)
    if (!isTRUE(val))
        return(val)
    TRUE
}


## NO_TESTS
#' Check values for Intervals labels
#'
#' Checks:
#' \itemize{
#'   \item is list of integer vectors of length 2
#'   \item no NAs, except element 1 of first item, and element 2 of last item
#'   \item second element greater than first
#'   \item pairs do not overlap
#' }
#' 
#' @inheritParams chk_label_values_categories
#' @param x A list of integer vectors, 
#' each of which has length 2.
#'
#' @examples
#' x <- list(c(100L, NA), c(0L, 5L), c(NA, 0L), c(5L, 50L))
#' chk_label_values_intervals(x = x, name = "x")
#' @export
chk_label_values_intervals <- function(x, name) {
    ## x is list
    val <- chk_is_list(x = x,
                       name = name)
    if (!isTRUE(val))
        return(val)
    ## all items in list are integer
    val <- chk_items_integer(x = x,
                             name = name)
    if (!isTRUE(val))
        return(val)
    ## all items in list have length 2
    val <- chk_items_length_k(x = x,
                              k = 2L,
                              name = name)
    if (!isTRUE(val))
        return(val)
    ## no NAs except possibly first element of first item
    ## and second element of last item
    n <- length(x)
    val <- chk_items_no_na(x = x,
                           except = list(c(1L, 1L), c(n, 2L)),
                           name = name)
    if (!isTRUE(val))
        return(val)
    ## second element greater than first element
    val <- chk_items_increasing(x = x,
                                strict = TRUE,
                                name = name)
    if (!isTRUE(val))
        return(val)
    ## pairs do not overlap
    val <- chk_no_overlap_intervals(x = x,
                                    name = name)
    if (!isTRUE(val))
        return(val)
    TRUE
}


## NO_TESTS
#' Check values for Quantities labels
#'
#' Checks:
#' \itemize{
#'   \item is list of integer vectors of length 2
#'   \item no NAs, except element 1 of first item, and element 2 of last item
#'   \item second element greater than or equal to first
#'   \item pairs do not overlap
#' }
#' 
#' @inheritParams chk_label_values_categories
#' @param x A list of integer vectors, 
#' each of which has length 2.
#'
#' @examples
#' x <- list(c(100L, NA), c(0L, 4L), c(NA, -1L), c(5L, 49L))
#' chk_label_values_quantities(x = x, name = "x")
#' @export
chk_label_values_quantities <- function(x, name) {
    ## x is list
    val <- chk_is_list(x = x,
                       name = name)
    if (!isTRUE(val))
        return(val)
    ## all items in list are integer
    val <- chk_items_integer(x = x,
                             name = name)
    if (!isTRUE(val))
        return(val)
    ## all items in list have length 2
    val <- chk_items_length_k(x = x,
                              k = 2L,
                              name = name)
    if (!isTRUE(val))
        return(val)
    ## no NAs except possibly first element of first item
    ## and second element of last item
    n <- length(x)
    val <- chk_items_no_na(x = x,
                           except = list(c(1L, 1L), c(n, 2L)),
                           name = name)
    if (!isTRUE(val))
        return(val)
    ## second element greater than or equal to first element
    val <- chk_items_increasing(x = x,
                                strict = FALSE,
                                name = name)
    if (!isTRUE(val))
        return(val)
    ## pairs do not overlap
    val <- chk_no_overlap_quantities(x = x,
                                     name = name)
    if (!isTRUE(val))
        return(val)
    TRUE
}



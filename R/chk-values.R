
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
#' chk_values_categories(x = x, name = "x")
#' @export
chk_values_categories <- function(x, name) {
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
    ## no blanks
    val <- chk_no_blanks(x = x,
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


## Specialised classes --------------------------------------------------------

## NO_TESTS
#' Check values for Triangles labels
#'
#' Checks:
#' \itemize{
#'   \item is character
#'   \item no duplicates
#'   \item values belong to set {NA, "Lower", "Upper"}
#' }
#' 
#' @inheritParams chk_values_categories
#'
#' @examples
#' x <- c("Upper", "Lower")
#' chk_values_triangles(x = x, name = "x")
#' @export
chk_values_triangles <- function(x, name) {
    expected_set <- c("Lower", "Upper", NA)
    ## is character
    val <- chk_is_character(x = x,
                            name = name)
    if (!isTRUE(val))
        return(val)
    ## no duplicates
    val <- chk_no_duplicates(x = x,
                             name = name)
    if (!isTRUE(val))
        return(val)
    ## members of expected set
    is_in_set <- x %in% expected_set
    i_not_in_set <- match(FALSE, is_in_set, nomatch = 0L)
    if (i_not_in_set > 0L)
        return(gettextf("'%s' has unexpected value : \"%s\"",
                        name, x[[i_not_in_set]]))
    TRUE
}

## NO_TESTS
#' Check values for Directions labels
#'
#' Checks:
#' \itemize{
#'   \item is character
#'   \item no duplicates
#'   \item values belong to set {NA, "In", "Out"}
#' }
#' 
#' @inheritParams chk_values_categories
#'
#' @examples
#' x <- c("In", "Out")
#' chk_values_directions(x = x, name = "x")
#' @export
chk_values_directions <- function(x, name) {
    expected_set <- c("In", "Out", NA)
    ## is character
    val <- chk_is_character(x = x,
                            name = name)
    if (!isTRUE(val))
        return(val)
    ## no duplicates
    val <- chk_no_duplicates(x = x,
                             name = name)
    if (!isTRUE(val))
        return(val)
    ## members of expected set
    is_in_set <- x %in% expected_set
    i_not_in_set <- match(FALSE, is_in_set, nomatch = 0L)
    if (i_not_in_set > 0L)
        return(gettextf("'%s' has unexpected value : \"%s\"",
                        name, x[[i_not_in_set]]))
    TRUE
}

## NO_TESTS
#' Check values for Quantiles labels
#'
#' Checks:
#' \itemize{
#'   \item is character
#'   \item no duplicates
#'   \item all elements have format "<number>%" where 0 <= number <= 100
#' }
#' 
#' @inheritParams chk_values_categories
#'
#' @examples
#' x <- c("2.5%", "50%", "97.5%")
#' chk_values_quantiles(x = x, name = "x")
#' @export
chk_values_quantiles <- function(x, name) {
    ## is character
    val <- chk_is_character(x = x,
                            name = name)
    if (!isTRUE(val))
        return(val)
    ## no duplicates
    val <- chk_no_duplicates(x = x,
                             name = name)
    if (!isTRUE(val))
        return(val)
    ## have correct format for quantiles
    val <- chk_valid_quantile(x = x,
                              name = name)
    if (!isTRUE(val))
        return(val)
    TRUE
}


## Numeric --------------------------------------------------------------------

## NO_TESTS
#' Check values for Integer labels
#'
#' Specialised version of Intervals labels.
#'
#' Checks:
#' \itemize{
#'   \item is list of integer vectors of length 2
#'   \item second element of each vector one greater than first
#'   \item intervals do not overlap
#' }
#' 
#' @inheritParams chk_values_categories
#' @param x A list of integer vectors, 
#' each of which has length 2.
#'
#' @examples
#' x <- list(c(100L, NA), c(0L, 1L), c(NA, 0L), c(5L, 6L))
#' chk_values_integers(x = x, name = "x")
#' @export
chk_values_integers <- function(x, name) {
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
    ## second element one greater than first element
    val <- chk_items_one_greater(x = x,
                                 name = name)
    if (!isTRUE(val))
        return(val)
    ## values do not overlap
    val <- chk_no_overlap_intervals(x = x,
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
#'   \item second element of each vector greater than first
#'   \item intervals do not overlap
#' }
#' 
#' @inheritParams chk_values_categories
#' @param x A list of integer vectors, 
#' each of which has length 2.
#'
#' @examples
#' x <- list(c(100L, NA), c(0L, 5L), c(NA, 0L), c(5L, 50L))
#' chk_values_intervals(x = x, name = "x")
#' @export
chk_values_intervals <- function(x, name) {
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
    ## second element greater than first element
    val <- chk_items_increasing(x = x,
                                strict = TRUE,
                                name = name)
    if (!isTRUE(val))
        return(val)
    ## values do not overlap
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
#'   \item second element of each vector greater than or equal to first
#'   \item pairs do not overlap
#' }
#' 
#' @inheritParams chk_values_categories
#' @param x A list of integer vectors, 
#' each of which has length 2.
#'
#' @examples
#' x <- list(c(100L, NA), c(0L, 4L), c(NA, -1L), c(5L, 49L))
#' chk_values_quantities(x = x, name = "x")
#' @export
chk_values_quantities <- function(x, name) {
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
    ## second element greater than or equal to first element
    val <- chk_items_increasing(x = x,
                                strict = FALSE,
                                name = name)
    if (!isTRUE(val))
        return(val)
    ## values do not overlap
    val <- chk_no_overlap_quantities(x = x,
                                     name = name)
    if (!isTRUE(val))
        return(val)
    TRUE
}


## Calendar -------------------------------------------------------------------

## NO_TESTS
#' Check values for Quarters labels
#'
#' Checks:
#' \itemize{
#'   \item is list of date vectors of length 2
#'   \item first element is first day of quarter
#'   \item second element is one quarter later than first (if neither NA)
#'   \item intervals do not overlap
#' }
#' 
#' @inheritParams chk_values_categories
#' @param x A list of integer vectors, 
#' each of which has length 2.
#'
#' @examples
#' x <- list(as.Date(c("2000-01-01", "2000-04-01")),
#'           as.Date(c(NA, "2000-01-01")),
#'           as.Date(c(NA, NA)))
#' chk_values_quarters(x = x, name = "x")
#' @export
chk_values_quarters <- function(x, name) {
    ## x is list
    val <- chk_is_list(x = x,
                       name = name)
    if (!isTRUE(val))
        return(val)
    ## all items in list are Date
    val <- chk_items_date(x = x,
                          name = name)
    if (!isTRUE(val))
        return(val)
    ## all items in list have length 2
    val <- chk_items_length_k(x = x,
                              k = 2L,
                              name = name)
    if (!isTRUE(val))
        return(val)
    for (i in seq_along(x)) {
        item <- x[[i]]
        ## both elements are first day of quarter
        for (j in 1:2) {
            el <- item[[j]]
            if (!is.na(el)) {
                month <- format(el, "%m")
                day <- format(el, "%d")
                month_valid <- month %in% c("01", "04", "07", "10")
                day_valid <- identical(day, "01")
                if (!month_valid || !day_valid)
                    return(gettextf(paste("problem with item %d of '%s' : element %d [\"%s\"]",
                                          "is not first day of quarter"),
                                    i, name, j, el))
            }
        }
        ## second element is one quarter later than first
        first <- item[[1L]]
        second <- item[[2L]]
        if (!is.na(first) && !is.na(second)) {
            second_expected <- seq.Date(from = first,
                                        by = "quarter",
                                        length.out = 2L)[[2L]]
            if (!identical(second, second_expected))
                return(gettextf(paste("problem with item %d of '%s' : second element [\"%s\"]",
                                      "not one quarter later than first element [\"%s\"]"),
                                i, name, second, first))
        }
    }
    ## values do not overlap
    val <- chk_no_overlap_intervals(x = x,
                                    name = name)
    if (!isTRUE(val))
        return(val)
    TRUE
}


## NO_TESTS
#' Check values for Months labels
#'
#' Checks:
#' \itemize{
#'   \item is list of date vectors of length 2
#'   \item both elements are first day of month
#'   \item second element is one month later than first (if neither NA)
#'   \item intervals do not overlap
#' }
#' 
#' @inheritParams chk_values_categories
#' @param x A list of integer vectors, 
#' each of which has length 2.
#'
#' @examples
#' x <- list(as.Date(c("2000-01-01", "2000-02-01")),
#'           as.Date(c(NA, "2000-01-01")),
#'           as.Date(c(NA, NA)))
#' chk_values_months(x = x, name = "x")
#' @export
chk_values_months <- function(x, name) {
    ## x is list
    val <- chk_is_list(x = x,
                       name = name)
    if (!isTRUE(val))
        return(val)
    ## all items in list are Date
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
    for (i in seq_along(x)) {
        item <- x[[i]]
        ## both elements are first day of month
        for (j in 1:2) {
            el <- item[[j]]
            if (!is.na(el)) {
                day <- format(el, "%d")
                day_valid <- identical(day, "01")
                if (!day_valid)
                    return(gettextf(paste("problem with item %d of '%s' : element %d [\"%s\"]",
                                          "is not first day of month"),
                                    i, name, j, el))
            }
        }
        ## second element is one month later than first
        first <- item[[1L]]
        second <- item[[2L]]
        if (!is.na(first) && !is.na(second)) {
            second_expected <- seq.Date(from = first,
                                        by = "month",
                                        length.out = 2L)[[2L]]
            if (!identical(second, second_expected))
                return(gettextf(paste("problem with item %d of '%s' : second element [\"%s\"]",
                                      "not one month later than first element [\"%s\"]"),
                                i, name, second, first))
        }
    }
    ## values do not overlap
    val <- chk_no_overlap_intervals(x = x,
                                    name = name)
    if (!isTRUE(val))
        return(val)
    TRUE
}


## NO_TESTS
#' Check values for DatePoints labels
#'
#' Checks:
#' \itemize{
#'   \item is Date vector
#'   \item no duplicates
#' }
#' 
#' @inheritParams chk_values_categories
#' @param x A vector of class \code{\link[base]{Date}}.
#'
#' @examples
#' x <- as.Date(c("2000-05-01", NA, "2000-02-01"))
#' chk_values_datepoints(x = x, name = "x")
#' @export
chk_values_datepoints <- function(x, name) {
    ## is date
    val <- chk_is_date(x = x,
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
#' Check values for DateRanges labels
#'
#' Checks:
#' \itemize{
#'   \item is list of date vectors of length 2
#'   \item second element is later than first (if neither NA)
#'   \item intervals do not overlap
#' }
#' 
#' @inheritParams chk_values_categories
#' @param x A list of integer vectors, 
#' each of which has length 2.
#'
#' @examples
#' x <- list(as.Date(c("2000-01-03", "2000-01-03")),
#'           as.Date(c(NA, "2000-01-02")),
#'           as.Date(c(NA, NA)))
#' chk_values_dateranges(x = x, name = "x")
#' @export
chk_values_dateranges <- function(x, name) {
    ## x is list
    val <- chk_is_list(x = x,
                       name = name)
    if (!isTRUE(val))
        return(val)
    ## all items in list are Date
    val <- chk_items_date(x = x,
                             name = name)
    if (!isTRUE(val))
        return(val)
    ## all items in list have length 2
    val <- chk_items_length_k(x = x,
                              k = 2L,
                              name = name)
    if (!isTRUE(val))
        return(val)
    ## second element is later than first
    for (i in seq_along(x)) {
        item <- x[[i]]
        first <- item[[1L]]
        second <- item[[2L]]
        if (!is.na(first) && !is.na(second)) {
            if (second <= first)
                return(gettextf(paste("problem with item %d of '%s' : second element [\"%s\"]",
                                      "equal to or less than first element [\"%s\"]"),
                                i, name, second, first))
        }
    }
    ## values do not overlap
    val <- chk_no_overlap_intervals(x = x,
                                    name = name)
    if (!isTRUE(val))
        return(val)
    TRUE
}

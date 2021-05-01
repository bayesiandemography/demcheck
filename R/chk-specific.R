
## Highly specialised checks.

## HAS_TESTS
#' Check that 'i_comp_type_self' consistent with
#' 'indices_orig_self' and 'i_direction_self'
#'
#' @param i_comp_type_self Integer between 1 and 4
#' @param indices_orig_self Integer vector
#' @param i_direction_self Integer scalar
#'
#' @examples
#' chk_comp_type_indices(i_comp_type_self = 3L,
#'                       indices_orig_self = 2L,
#'                       i_direction_self = 0L)
#' @export
chk_comp_type_indices <- function(i_comp_type_self, indices_orig_self, i_direction_self) {
    is_orig <- identical(i_comp_type_self, 3L)
    is_pool <- identical(i_comp_type_self, 4L)
    has_ind_orig <- !identical(indices_orig_self, 0L)
    has_ind_direction <- !identical(i_direction_self, 0L)
    if (is_orig && !has_ind_orig)
        return(gettextf("'%s' is %d but '%s' is %d",
                        "i_comp_type_self",
                        i_comp_type_self,
                        "indices_orig_self",
                        0L))
    if (!is_orig && has_ind_orig)
        return(gettextf("'%s' is %d but '%s' is not %d",
                        "i_comp_type_self",
                        i_comp_type_self,
                        "indices_orig_self",
                        0L))
    if (is_pool && !has_ind_direction)
        return(gettextf("'%s' is %d but '%s' is %d",
                        "i_comp_type_self",
                        i_comp_type_self,
                        "i_direction_self",
                        0L))
    if (!is_pool && has_ind_direction)
        return(gettextf("'%s' is %d but '%s' is not %d",
                        "i_comp_type_self",
                        i_comp_type_self,
                        "i_direction_self",
                        0L))
    TRUE
}


## HAS_TESTS
#' Check that a list describing general transitions between
#' states is valid
#'
#' @param x A list if character vectors.
#' @param name The name for \code{x} to be used
#' in error messages.
#'
#' @examples
#' x <- list(S = "I",
#'           I = c("R", "D"))
#' chk_trans_list(x, name = "x")
#' @export
chk_trans_list <- function(x, name) {
    if (!is.list(x))
        stop(gettextf("'%s' is not a list",
                      name))
    val <- chk_names_complete(x = x,
                              name = name)
    if (!isTRUE(val))
        return(val)
    names_x <- names(x)
    for (i in seq_along(x)) {
        element <- x[[i]]
        name_element <- names_x[[i]]
        if (is.null(element)) {
            next
        }
        else if (is.character(element)) {
            if (anyNA(element))
                return(gettextf("element \"%s\" of '%s' has NAs",
                                name_element, name))
            if (any(!nzchar(element)))
                return(gettextf("element \"%s\" of '%s' has blanks",
                                name_element, name))
            if (any(duplicated(element)))
                return(gettextf("element \"%s\" of '%s' has duplicates",
                                name_element, name))
            dest_not_found <- match(element, names_x, nomatch = 0L) == 0L
            if (any(dest_not_found)) {
                i_not_found <- match(TRUE, dest_not_found)
                return(gettextf(paste("value \"%s\" in element \"%s\" of '%s' invalid :",
                                      "\"%s\" is not the name of an element of '%s'"),
                                element[[i_not_found]],
                                name_element,
                                name,
                                element[[i_not_found]],
                                name))
            }
        }
        else {
            return(gettextf("element \"%s\" of '%s' has class \"%s\"",
                            name_element, name, class(element)))
        }
    }
    TRUE    
}


## HAS_TESTS
#' Check 'map_dim'
#'
#' Validity tests for a \code{\link[=err_tdy_map_dim]{map_dim}}
#' that only require the object itself.
#'
#' @inheritParams chk_trans_list
#' @param x A vector of non-negative integers,
#' unique apart from any zeros.
#'
#' @seealso \code{\link{chk_map_pos}}
#'
#' @examples
#' chk_map_dim(x = c(3L, 1L, 0L),
#'             name = "map_dim")
#' @export
chk_map_dim <- function(x, name) {
    val <- chk_is_integer(x = x,
                          name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_not_na_vector(x = x,
                             name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_non_negative_vector(x = x,
                                   name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_has_nonzero(x = x,
                           name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_nonzero_unique(x = x,
                              name = name)
    if (!isTRUE(val))
        return(val)
    TRUE
}


## HAS_TESTS
#' Check 'map_dim'
#'
#' Validity tests for a \code{\link[=err_tdy_map_pos]{map_pos}}
#' that only require the object itself.
#'
#' @inheritParams chk_trans_list
#' @param x A list of integer vectors.
#'
#' @seealso \code{\link{chk_map_pos}}
#'
#' @examples
#' x <- list(c(2L, 0L), 1:3)
#' chk_map_pos(x = x,
#'             name = "x")
#' @export
chk_map_pos <- function(x, name) {
    val <- chk_is_class_list(x = x,
                             name = name,
                             class = "integer")
    if (!isTRUE(val))
        return(val)
    for (i in seq_along(x)) {
        x_i <- x[[i]]
        name_i <- sprintf("element %d of '%s'",
                          i, name)
        val <- chk_is_integer(x = x_i,
                              name = name_i)
        if (!isTRUE(val))
            return(val)
        val <- chk_positive_length(x = x_i,
                                   name = name_i)
        if (!isTRUE(val))
            return(val)
        val <- chk_not_na_vector(x = x_i,
                                 name = name_i)
        if (!isTRUE(val))
            return(val)
        val <- chk_non_negative_vector(x = x_i,
                                       name = name_i)
        if (!isTRUE(val))
            return(val)
    }
    TRUE
}


#' Check that labels do not have any open age groups,
#' assuming that 'open_last' is FALSE
#'
#' @param x A character vector with age labels.
#'
#' @examples
#' chk_no_open_age(c("0-4", "5", "100"))
#'
#' @export
chk_no_open_age <- function(x) {
    p_open <- "^[0-9]+\\+$"
    is_open <- grepl(p_open, x)
    i_open <- match(TRUE, is_open, nomatch = 0L)
    if (i_open > 0L)
        return(gettextf("'%s' is %s but age group \"%s\" is open",
                        "open_last", "FALSE", x[[i_open]]))
    TRUE
}


#' Check that labels do not have any open cohorts,
#' assuming that 'open_first' is FALSE
#'
#' @param x A character vector with cohort labels.
#'
#' @examples
#' chk_no_open_cohort(c("2000-2005", "2005-2010", "2010-2015"))
#'
#' @export
chk_no_open_cohort <- function(x) {
    p_open <- "^<-?[0-9]+$"
    is_open <- grepl(p_open, x)
    i_open <- match(TRUE, is_open, nomatch = 0L)
    if (i_open > 0L)
        return(gettextf("'%s' is %s but cohort \"%s\" is open",
                        "open_first", "FALSE", x[[i_open]]))
    TRUE
}


#' Check that pairs of numbers specifying 
#' labels do not overlap
#'
#' Look for cases where the first pair starts at or below the
#' second pair, and finished in or above the second pair.
#' Function 'outer' covers all combinations of pairs,
#' in both orders, so looking only at overlaps from below
#' finds all overlaps.
#'
#' Intervals allow the second number
#' of the lower pair to equal the first number of the upper pair,
#' while quantities do not.
#' 
#' @inheritParams chk_trans_list
#' @param x A list of integer vectors of length 2.
#'
#' @examples
#' x <- list(c(NA, NA), c(NA, 20L), c(30L, NA), c(20L, 30L)) 
#' chk_no_overlap_pairs(x = x,
#'                      name = "x")
#'
#' x <- list(as.Date(c(NA, "2020-10-01")),
#'           as.Date(c(NA, NA)),
#'           as.Date(c("2020-01-01", "2020-04-01")))
#' chk_no_overlap_quarters(x = x,
#'                         name = "x")
#'
#' x <- list(as.Date(c(NA, "2020-10-01")),
#'           as.Date(c(NA, NA)),
#'           as.Date(c("2020-01-01", "2020-02-01")))
#' chk_no_overlap_months(x = x,
#'                       name = "x")
#' @name chk_no_overlap
NULL

#' @rdname chk_no_overlap
#' @export
chk_no_overlap_pairs <- function(x, name) {
    e1 <- sapply(x, `[[`, 1L)
    e2 <- sapply(x, `[[`, 2L)
    is_both_na <- is.na(e1) & is.na(e2)
    if (sum(is_both_na) > 1L)
        return(gettextf("problem with elements of '%s' : more than one item where both elements are NA",
                        name))
    x <- x[!is_both_na]
    n <- length(x)
    if (n >= 2L) {
        e1 <- e1[!is_both_na]
        e2 <- e2[!is_both_na]
        e1[is.na(e1)] <- min(e1, e2, na.rm = TRUE) - 1L
        e2[is.na(e2)] <- max(e1, e2, na.rm = TRUE) + 1L
        is_invalid_starts_below <- outer(e1, e1, `<`) & outer(e2, e1, `>`)
        is_invalid_starts_same <- outer(e1, e1, `==`)
        is_invalid <- is_invalid_starts_below | is_invalid_starts_same
        diag(is_invalid) <- FALSE
        if (any(is_invalid)) {
            indices_invalid <- which(is_invalid, arr.ind = TRUE)
            first <- x[[indices_invalid[1L, 1L]]]
            second <- x[[indices_invalid[1L, 2L]]]
            lab_first <- make_label_intervals(first)
            lab_second <- make_label_intervals(second)
            return(gettextf("problem with elements of '%s' : \"%s\" overlaps with \"%s\"",
                            name, lab_first, lab_second))
        }
    }
    TRUE
}

#' @rdname chk_no_overlap
#' @export
chk_no_overlap_months <- function(x, name) {
    x_int <- lapply(x, as.integer)
    e1 <- sapply(x_int, `[[`, 1L)
    e2 <- sapply(x_int, `[[`, 2L)
    is_both_na <- is.na(e1) & is.na(e2)
    if (sum(is_both_na) > 1L)
        return(gettextf("problem with elements of '%s' : more than one item where both elements are NA",
                        name))
    x <- x[!is_both_na]
    n <- length(x)
    if (n >= 2L) {
        e1 <- e1[!is_both_na]
        e2 <- e2[!is_both_na]
        e1[is.na(e1)] <- min(e1, e2, na.rm = TRUE) - 1L
        e2[is.na(e2)] <- max(e1, e2, na.rm = TRUE) + 1L
        is_invalid_starts_below <- outer(e1, e1, `<`) & outer(e2, e1, `>`)
        is_invalid_starts_same <- outer(e1, e1, `==`)
        is_invalid <- is_invalid_starts_below | is_invalid_starts_same
        diag(is_invalid) <- FALSE
        if (any(is_invalid)) {
            indices_invalid <- which(is_invalid, arr.ind = TRUE)
            first <- x[[indices_invalid[1L, 1L]]]
            second <- x[[indices_invalid[1L, 2L]]]
            lab_first <- make_label_months(first)
            lab_second <- make_label_months(second)
            return(gettextf("problem with elements of '%s' : \"%s\" overlaps with \"%s\"",
                            name, lab_first, lab_second))
        }
    }
    TRUE
}

#' @rdname chk_no_overlap
#' @export
chk_no_overlap_quarters <- function(x, name) {
    x_int <- lapply(x, as.integer)
    e1 <- sapply(x_int, `[[`, 1L)
    e2 <- sapply(x_int, `[[`, 2L)
    is_both_na <- is.na(e1) & is.na(e2)
    if (sum(is_both_na) > 1L)
        return(gettextf("problem with elements of '%s' : more than one item where both elements are NA",
                        name))
    x <- x[!is_both_na]
    n <- length(x)
    if (n >= 2L) {
        e1 <- e1[!is_both_na]
        e2 <- e2[!is_both_na]
        e1[is.na(e1)] <- min(e1, e2, na.rm = TRUE) - 1L
        e2[is.na(e2)] <- max(e1, e2, na.rm = TRUE) + 1L
        is_invalid_starts_below <- outer(e1, e1, `<`) & outer(e2, e1, `>`)
        is_invalid_starts_same <- outer(e1, e1, `==`)
        is_invalid <- is_invalid_starts_below | is_invalid_starts_same
        diag(is_invalid) <- FALSE
        if (any(is_invalid)) {
            indices_invalid <- which(is_invalid, arr.ind = TRUE)
            first <- x[[indices_invalid[1L, 1L]]]
            second <- x[[indices_invalid[1L, 2L]]]
            lab_first <- make_label_quarters(first)
            lab_second <- make_label_quarters(second)
            return(gettextf("problem with elements of '%s' : \"%s\" overlaps with \"%s\"",
                            name, lab_first, lab_second))
        }
    }
    TRUE
}


#' Check that open intervals do not overlap with 'break_min' or 'break_max'
#'
#' \code{chk_open_left_le_break_min} assumes that intervals are open
#' on the left, and checks that they do not exceed \code{break_min}.
#'
#' \code{chk_open_right_ge_break_max} assumes that intervals open
#' on the right, and checks that they do not exceed \code{break_max}.
#'
#' @param labels A character vector with age labels.
#' @param int_low Lower limits of intervals.
#' @param int_up Upper limits of intervals.
#' @param is_open Logical vector - whether each interval is open
#' @param break_min Lowest break.
#' @param break_max Highest break.
#'
#' @examples
#' chk_open_left_le_break_min(labels = c("2005-2010", "<2000", NA),
#'                            int_up = c(2010, 2000, NA),
#'                            is_open = c(FALSE, TRUE, FALSE),
#'                            break_min = 2000)
#' chk_open_right_ge_break_max(labels = c("2005-2010", "2010+", NA),
#'                             int_low = c(2005, 2010, NA),
#'                             is_open = c(FALSE, TRUE, FALSE),
#'                             break_max = 2010)
#' @name chk_open_left_le_break_min
NULL

#' @rdname chk_open_left_le_break_min
#' @export
chk_open_left_le_break_min <- function(labels, int_up, is_open, break_min) {
    if (!is.null(break_min)) {
        is_gt_break_min <- !is.na(int_up) & (int_up > break_min) & is_open
        i_gt_break_min <- match(TRUE, is_gt_break_min, nomatch = 0L)
        if (i_gt_break_min > 0L)
            return(gettextf("upper limit of open interval \"%s\" is greater than '%s' [%d]",
                            labels[[i_gt_break_min]], "break_min", break_min))
    }
    TRUE
}

#' @rdname chk_open_left_le_break_min
#' @export
chk_open_right_ge_break_max <- function(labels, int_low, is_open, break_max) {
    if (!is.null(break_max)) {
        is_lt_break_max <- !is.na(int_low) & (int_low < break_max) & is_open
        i_lt_break_max <- match(TRUE, is_lt_break_max, nomatch = 0L)
        if (i_lt_break_max > 0L)
            return(gettextf("lower limit of open interval \"%s\" is less than '%s' [%d]",
                            labels[[i_lt_break_max]], "break_max", break_max))
    }
    TRUE
}




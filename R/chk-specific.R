
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
            if (any(is.na(element)))
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
        val <- chk_non_negative_vector(x = x_i,
                                       name = name_i)
        if (!isTRUE(val))
            return(val)
    }
    TRUE
}

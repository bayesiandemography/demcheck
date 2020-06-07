
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
#' Check that a \code{\link[=err_tdy_map_dim]{map_dim}}
#' argument is valid.
#'
#' @param map_dim A vector of non-negative integers,
#' unique apart from any zeros, the same length as
#' \code{dim_self}.
#' @param dim_self The dimensions of \code{self}.
#' @param dim_oth The dimensions of \code{oth}.
#'
#' @examples
#' map_dim <- c(3, 1, 0)
#' dim_self <- c(4L, 6L, 2L)
#' dim_oth <- c(6L, 3L, 4L, 3L)
#' chk_map_dim(map_dim = map_dim,
#'             dim_self = dim_self,
#'             dim_oth = dim_oth)
#' @export
chk_map_dim <- function(map_dim, dim_self, dim_oth) {
    val <- chk_is_integer(x = map_dim,
                          name = "map_dim")
    if (!isTRUE(val))
        return(val)
    val <- chk_non_negative_vector(x = map_dim,
                                   name = "map_dim")
    if (!isTRUE(val))
        return(val)
    val <- chk_has_nonzero(x = map_dim,
                           name = "map_dim")
    if (!isTRUE(val))
        return(val)
    val <- chk_nonzero_unique(x = map_dim,
                              name = "map_dim")
    if (!isTRUE(val))
        return(val)
    val <- chk_length_same(x1 = map_dim,
                           x2 = dim_self,
                           name1 = "map_dim",
                           name2 = "dim_self")
    if (!isTRUE(val))
        return(val)
    val <- chk_all_x1_in_x2(x1 = map_dim,
                            x2 = seq_along(dim_oth),
                            name1 = "map_dim",
                            name2 = "seq_along(dim_oth)",
                            exclude_zero = TRUE)
    if (!isTRUE(val))
        return(val)
    TRUE
}

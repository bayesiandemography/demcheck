
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


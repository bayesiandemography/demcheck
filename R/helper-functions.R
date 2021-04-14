
string_subset_vec <- function(vec) {
    n <- length(vec)
    if (n <= 3L)
        paste(vec, collapse = ", ")
    else
        paste(paste(vec[1:3], collapse = ", "),
              "...",
              sep = ", ")
}



## HAS_TESTS
make_label_intervals <- function(x) {
    x1 <- x[[1L]]
    x2 <- x[[2L]]
    if (is.na(x1))
        paste0("--", x2)
    else if (is.na(x2))
        paste0(x1, "--")
    else
        paste0(x1, "--", x2)
}


## HAS_TESTS
make_label_quantities <- function(x) {
    x1 <- x[[1L]]
    x2 <- x[[2L]]
    if (is.na(x1))
        paste0("<=", x2)
    else if (is.na(x2))
        paste0(x1, "+")
    else if (x1 == x2)
        as.character(x1)
    else
        paste0(x1, "-", x2)
}
    
    
    
                   

    
    
    

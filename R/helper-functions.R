
string_subset_vec <- function(vec) {
    n <- length(vec)
    if (n <= 3L)
        paste(vec, collapse = ", ")
    else
        paste(paste(vec[1:3], collapse = ", "),
              "...",
              sep = ", ")
}
    
    
    

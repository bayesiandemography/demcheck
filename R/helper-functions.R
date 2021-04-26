
string_subset_vec <- function(vec) {
    n <- length(vec)
    if (n <= 3L)
        paste(vec, collapse = ", ")
    else
        paste(paste(vec[1:3], collapse = ", "),
              "...",
              sep = ", ")
}

## make_label -----------------------------------------------------------------

## HAS_TESTS
make_label_intervals <- function(x) {
    x1 <- x[[1L]]
    x2 <- x[[2L]]
    is_na_1 <- is.na(x1)
    is_na_2 <- is.na(x2)
    if (is_na_1 && is_na_2)
        NA_character_
    else if (is_na_1 && !is_na_2)
        paste0("--", x2)
    else if (!is_na_1 && is_na_2)
        paste0(x1, "--")
    else
        paste0(x1, "--", x2)
}


## HAS_TESTS
make_label_quantities <- function(x) {
    x1 <- x[[1L]]
    x2 <- x[[2L]]
    is_na_1 <- is.na(x1)
    is_na_2 <- is.na(x2)
    if (is_na_1 && is_na_2)
        NA_character_
    else if (is_na_1 && !is_na_2)
        paste0("<", x2 + 1L)
    else if (!is_na_1 && is_na_2)
        paste0(x1, "+")
    else if (x1 == x2)
        as.character(x1)
    else
        paste0(x1, "-", x2)
}

## HAS_TESTS
make_label_quarters <- function(x) {
    x1 <- x[[1L]]
    x2 <- x[[2L]]
    y1 <- format(x1, "%Y")
    y2 <- format(x2, "%Y")
    q1 <- quarters(x1)
    q2 <- quarters(x2)
    s1 <- paste(y1, q1)
    s2 <- paste(y2, q2)
    is_na_1 <- is.na(x1)
    is_na_2 <- is.na(x2)
    if (is_na_1 && is_na_2)
        NA_character_
    else if (is_na_1 && !is_na_2)
        paste0("<", s2)
    else if (!is_na_1 && is_na_2)
        paste0(s1, "+")
    else
        s1
}

## HAS_TESTS
make_label_months <- function(x) {
    x1 <- x[[1L]]
    x2 <- x[[2L]]
    s1 <- format(x1, "%Y %b")
    s2 <- format(x2, "%Y %b")
    is_na_1 <- is.na(x1)
    is_na_2 <- is.na(x2)
    if (is_na_1 && is_na_2)
        NA_character_
    else if (is_na_1 && !is_na_2)
        paste0("<", s2)
    else if (!is_na_1 && is_na_2)
        paste0(s1, "+")
    else
        s1
}
    
    
    
                   

    
    
    


context("chk-single")

## chk_at_most_one_na_vector --------------------------------------------------

test_that("'chk_at_most_one_na_vector' returns TRUE with valid inputs", {
    expect_true(chk_at_most_one_na_vector(x = c(0L, NA, 1L), name = "x"))
    expect_true(chk_at_most_one_na_vector(x = c(0L, 1L), name = "x"))
    expect_true(chk_at_most_one_na_vector(x = character(), name = "x"))
    expect_true(chk_at_most_one_na_vector(x = NA, name = "x"))
})

test_that("'chk_at_most_one_na_vector' returns expected string with invalid inputs", {
    expect_identical(chk_at_most_one_na_vector(x = c(0L, 1L, NA, NA), name = "x"),
                     "'x' has 2 NAs")
})

## chk_at_most_one_na_list --------------------------------------------------

test_that("'chk_at_most_one_na_list' returns TRUE with valid inputs", {
    expect_true(chk_at_most_one_na_list(x = list(c(0L, NA, 1L), 1L), name = "x"))
    expect_true(chk_at_most_one_na_list(x = list(c(0L, 1L), c(NA, NA)), name = "x"))
    expect_true(chk_at_most_one_na_list(x = list(character()), name = "x"))
    expect_true(chk_at_most_one_na_list(x = list(c(NA, NA)), name = "x"))
})

test_that("'chk_at_most_one_na_list' returns expected string with invalid inputs", {
    expect_identical(chk_at_most_one_na_list(x = list(c(NA, NA), NA), name = "x"),
                     "'x' has 2 items where all elements are NAs")
})


## chk_is_date_equiv_scalar ---------------------------------------------------

test_that("'chk_is_date_equiv_scalar' returns TRUE with valid dates", {
    expect_true(chk_is_date_equiv_scalar(x = as.Date("2001-01-01"),
                                         name = "x"))
})

test_that("'chk_is_date_equiv_scalar' returns expected string with invalid dates", {
    expect_identical(chk_is_date_equiv_scalar(x = 1,
                                              name = "x"),
                     "'x' [\"1\"] not equivalent to date : 'origin' must be supplied")
    expect_identical(chk_is_date_equiv_scalar(x = "a",
                                              name = "x"),
                     "'x' [\"a\"] not equivalent to date : character string is not in a standard unambiguous format")
})

## chk_is_date_equiv_vector ---------------------------------------------------

test_that("'chk_is_date_equiv_vector' returns TRUE with valid dates", {
    expect_true(chk_is_date_equiv_vector(x = as.Date(c("2001-01-01", "2002-01-01")),
                                         name = "x"))
    expect_true(chk_is_date_equiv_vector(x = as.Date(character()),
                                         name = "x"))
})

test_that("'chk_is_date_equiv_vector' returns expected string with invalid dates", {
    expect_identical(chk_is_date_equiv_vector(x = 1,
                                              name = "x"),
                     "'x' [1] not equivalent to dates : 'origin' must be supplied")
    expect_identical(chk_is_date_equiv_vector(x = letters,
                                              name = "x"),
                     "'x' [a, b, c, ...] not equivalent to dates : character string is not in a standard unambiguous format")
    expect_identical(chk_is_date_equiv_vector(x = c("2000-01-01", "a-02-01"),
                                              name = "x"),
                     "value \"a-02-01\" in 'x' not equivalent to date")
})


## chk_is_integer_consec ------------------------------------------------------

test_that("'chk_is_integer_consec' returns TRUE with valid vector", {
    expect_true(chk_is_integer_consec(x = 1:10,
                                      name = "x"))
    expect_true(chk_is_integer_consec(x = seq.int(-8L, -4L),
                                      name = "x"))
    expect_true(chk_is_integer_consec(x = 1L,
                                      name = "x"))
    expect_true(chk_is_integer_consec(x = integer(),
                                      name = "x"))
})

test_that("'chk_is_integer_consec' returns expected message with invalid vector", {
    expect_identical(chk_is_integer_consec(x = c(1L, 3L),
                                           name = "x"),
                     "elements 1 [1] and 2 [3] of 'x' are not consecutive integers")
})


## chk_is_integer_equiv_vector ------------------------------------------------

test_that("'chk_is_integer_equiv_vector' returns TRUE with valid inputs", {
    expect_true(chk_is_integer_equiv_vector(x = c(1, NA, 22, -1), name = "x"))
    expect_true(chk_is_integer_equiv_vector(x = character(), name = "x"))
})

test_that("'chk_is_integer_equiv_vector' returns expected string with invalid input", {
    expect_identical(chk_is_integer_equiv_vector(x = c(0.2, 3),
                                                 name = "x"),
                     "value '0.2' in 'x' not equivalent to integer")
    expect_identical(chk_is_integer_equiv_vector(x = c(0, Inf),
                                                 name = "x"),
                     "value 'Inf' in 'x' not equivalent to integer")
    expect_identical(chk_is_integer_equiv_vector(x = c(0, Inf, 0.1),
                                                 name = "x"),
                     "value 'Inf' in 'x' not equivalent to integer")
    expect_identical(chk_is_integer_equiv_vector(x = "a",
                                                 name = "x"),
                     "value 'a' in 'x' not equivalent to integer")
})


## chk_items_date -------------------------------------------------------------

test_that("'chk_items_date' returns TRUE with valid inputs", {
    expect_true(chk_items_date(x = list(as.Date("2000-01-01"), as.Date(character())),
                                  name = "x"))
    expect_true(chk_items_date(x = list(), name = "x"))
})

test_that("'chk_items_date' returns expected string with invalid input", {
    expect_identical(chk_items_date(x = list(as.Date(character()), 0),
                                    name = "x"),
                     "item 2 of 'x' has class \"numeric\"")
})


## chk_items_increasing ----------------------------------------------------------

test_that("'chk_items_increasing' returns TRUE with valid inputs", {
    expect_true(chk_items_increasing(x = list(c(0L, 2L), integer(), c(0, NA, 2, 3)),
                                     strict = TRUE,
                                     name = "x"))
    expect_true(chk_items_increasing(x = list(c(0L, 0L, 2L), integer(), c(0, NA, 2, 3)),
                                     strict = FALSE,
                                     name = "x"))
    expect_true(chk_items_increasing(x = list(), strict = TRUE, name = "x"))
})

test_that("'chk_items_increasing' returns expected string with invalid input", {
    expect_identical(chk_items_increasing(x = list(0L, c(0L, 0L)),
                                          strict = TRUE,
                                          name = "x"),
                     "elements of item 2 of 'x' not strictly increasing")
    expect_identical(chk_items_increasing(x = list(0L, c(0L, -1L)),
                                          strict = FALSE,
                                          name = "x"),
                     "elements of item 2 of 'x' not increasing")
})


## chk_items_integer ----------------------------------------------------------

test_that("'chk_items_integer' returns TRUE with valid inputs", {
    expect_true(chk_items_integer(x = list(c(0L, 2L), integer()),
                                  name = "x"))
    expect_true(chk_items_integer(x = list(), name = "x"))
})

test_that("'chk_items_integer' returns expected string with invalid input", {
    expect_identical(chk_items_integer(x = list(0L, 0),
                                       name = "x"),
                     "item 2 of 'x' has class \"numeric\"")
})


## chk_items_length_k ---------------------------------------------------------

test_that("'chk_items_length_k' returns TRUE with valid inputs", {
    expect_true(chk_items_length_k(x = list(c(0L, 2L), c("a", "b")),
                                   k = 2L,
                                   name = "x"))
    expect_true(chk_items_length_k(x = list(), k = 2L, name = "x"))
})

test_that("'chk_items_length_k' returns expected string with invalid input", {
    expect_identical(chk_items_length_k(x = list(0L, 0),
                                        k = 2L,
                                        name = "x"),
                     "item 1 of 'x' has length 1")
})


## chk_items_no_na ------------------------------------------------------------

test_that("'chk_items_no_na' returns TRUE with valid inputs", {
    expect_true(chk_items_no_na(x = list(c(0L, 2L), NA, c("a", "b"), c(1, NA, 3)),
                                except = list(c(2L, 1L), c(4L, 2L)),
                                name = "x"))
    expect_true(chk_items_no_na(x = list(), except = list(), name = "x"))
})

test_that("'chk_items_no_na' returns expected string with invalid input", {
    expect_identical(chk_items_no_na(x = list(0L, NA, c(1, NA)),
                                     except = list(c(2L, 1L)),
                                     name = "x"),
                     "item 3 of 'x' has NA")
})


## chk_items_one_greater ------------------------------------------------------

test_that("'chk_items_one_greater' returns TRUE with valid inputs", {
    expect_true(chk_items_one_greater(x = list(c(0L, 1L),
                                               c(NA_integer_, NA_integer_),
                                               c(NA, -5L),
                                               c(10L, NA)),
                                      name = "x"))
    expect_true(chk_items_one_greater(x = list(), name = "x"))
})

test_that("'chk_items_one_greater' returns expected string with invalid input", {
    expect_identical(chk_items_one_greater(x = list(c(0L, 1L),
                                                    c(12L, 14L)),
                                           name = "x"),
                     "second element [14] of item 2 is not one greater than first element [12]")
})


## chk_no_duplicates ----------------------------------------------------------

test_that("'chk_no_duplicates' returns TRUE with valid inputs", {
    expect_true(chk_no_duplicates(x = c(0L, 2L, 1L, NA), name = "x"))
    expect_true(chk_no_duplicates(x = integer(), name = "x"))
})

test_that("'chk_no_duplicates' returns expected string with invalid input", {
    expect_identical(chk_no_duplicates(x = c(0L, 1L, 1L),
                                       name = "x"),
                     "element 3 of 'x' [1] is duplicate")
})


## chk_no_open_first ----------------------------------------------------------

test_that("'chk_no_open_first' returns TRUE with valid inputs", {
    expect_true(chk_no_open_first(x = c("2000 Feb", "2021 Mar", NA),
                                  name = "x",
                                  unit = "month"))
    expect_true(chk_no_open_first(x = c("2000 Q1", "2021 Q2", NA),
                                  name = "x",
                                  unit = "quarter"))
    expect_true(chk_no_open_first(x = c("2000", "2021", NA),
                                  name = "x",
                                  unit = "year"))
    expect_true(chk_no_open_first(x = character(),
                                  name = "x",
                                  unit = "year"))
})

test_that("'chk_no_open_first' returns expected string with invalid input", {
    expect_identical(chk_no_open_first(x = c("2000 Feb", "<2021 Mar", NA),
                                  name = "x",
                                  unit = "month"),
                     "'x' has open interval [\"<2021 Mar\"]")
    expect_identical(chk_no_open_first(x = c("2000 Q1", "<2021 Q2", NA),
                                  name = "x",
                                  unit = "quarter"),
                     "'x' has open interval [\"<2021 Q2\"]")
    expect_identical(chk_no_open_first(x = c("2000", "<2021", NA),
                                  name = "x",
                                  unit = "year"),
                     "'x' has open interval [\"<2021\"]")
})




## chk_nonzero_unique ---------------------------------------------------------

test_that("'chk_nonzero_unique' returns TRUE with valid inputs", {
    expect_true(chk_nonzero_unique(x = c(0L, 2L, 0L, 1L, 0L), name = "x"))
    expect_true(chk_nonzero_unique(x = 0L, name = "x"))
    expect_true(chk_nonzero_unique(x = integer(), name = "x"))
})

test_that("'chk_nonzero_unique' returns expected string with invalid input", {
    expect_identical(chk_nonzero_unique(x = c(0L, 1L, 1L),
                                        name = "x"),
                     "non-zero element of 'x' is duplicated : 1")
})

    



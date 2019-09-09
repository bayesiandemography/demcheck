
context("chk-composite")


## chk_all_class

test_that("'chk_all_class' returns TRUE with valid inputs", {
    expect_true(chk_all_class(x = list("a", "b"),
                              name = "x",
                              class = "character"))
    expect_true(chk_all_class(x = list(),
                              name = "x",
                              class = "character"))
})

test_that("'chk_all_class' returns expected message with invalid input", {
    expect_identical(chk_all_class(x = list("a", 1L),
                              name = "x",
                              class = "character"),
                     "element 2 of 'x' has class \"integer\" : should instead inherit from class \"character\"")
                
})


## chk_array_metadata_complete

test_that("'chk_array_metadata_complete' returns TRUE with valid array", {
    x <- array(0L,
               dim = 2:3,
               dimnames = list(A = 1:2, b = 1:3))
    expect_true(chk_array_metadata_complete(x = x,
                                            name = "x"))
    x <- array(0L,
               dim = c(0, 3),
               dimnames = list(A = character(), b = 1:3))
    expect_true(chk_array_metadata_complete(x = x,
                                            name = "x"))
})


## chk_character_complete

test_that("'chk_character_complete' returns TRUE with valid character vector", {
    expect_true(chk_character_complete(x = c("a", "b"),
                                       name = "x"))
    expect_true(chk_character_complete(x = character(),
                                       name = "x"))
})

test_that("'chk_character_complete' returns expected message with invalid character vector", {
    expect_identical(chk_character_complete(x = c("a", NA),
                                            name = "x"),
                     "'x' has NAs")
    expect_identical(chk_character_complete(x = "",
                                            name = "x"),
                     "'x' has blanks")
    expect_identical(chk_character_complete(x = c("a", "b", "a"),
                                            name = "x"),
                     "'x' has duplicate [\"a\"]")
})


## chk_dimnames_complete

test_that("'chk_dimnames_complete' returns TRUE with valid array", {
    x <- array(0L,
               dim = 2:3,
               dimnames = list(A = 1:2, b = 1:3))
    expect_true(chk_dimnames_complete(x = x,
                                      name = "x"))
    x <- array(0L,
               dim = c(0, 3),
               dimnames = list(A = character(), b = 1:3))
    expect_true(chk_dimnames_complete(x = x,
                                      name = "x"))
})

test_that("'chk_dimnames_complete' returns expected message with invalid argument", {
    x <- array(0L,
               dim = 2:3,
               dimnames = list(A = 1:2, b = 1:3))
    x_wrong <- x
    dimnames(x_wrong)[1] <- list(NULL)
    expect_identical(chk_dimnames_complete(x = x_wrong,
                                                 name = "x"),
                     "\"A\" dimension of 'x' does not have dimnames")
    x_wrong <- x
    dimnames(x_wrong)[[1]][1] <- NA
    expect_identical(chk_dimnames_complete(x = x_wrong,
                                                 name = "x"),
                     "dimnames for \"A\" dimension of 'x' have NAs")
    x_wrong <- x
    dimnames(x_wrong)[[1]][1] <- ""
    expect_identical(chk_dimnames_complete(x = x_wrong,
                                                 name = "x"),
                     "dimnames for \"A\" dimension of 'x' have blanks")
    x_wrong <- x
    dimnames(x_wrong)[[1]][1] <- 2
    expect_identical(chk_dimnames_complete(x = x_wrong,
                                           name = "x"),
                     "dimnames for \"A\" dimension of 'x' have duplicate [\"2\"]")
})


## chk_ge_age_min

test_that("'chk_ge_age_min' returns TRUE with valid inputs", {
    expect_true(chk_ge_age_min(age = 5L,
                               age_min = 0L,
                               date = as.Date("2005-01-02"),
                               dob = as.Date("2000-01-01"),
                               unit = "year"))
})

test_that("'chk_ge_age_min' returns expected message with invalid dates", {
    expect_identical(chk_ge_age_min(age = 5L,
                                    age_min = 10L,
                                    date = as.Date("2005-01-02"),
                                    dob = as.Date("2000-01-01"),
                                    unit = "year"),
                     "'date' [\"2005-01-02\"] and 'dob' [\"2000-01-01\"] imply an age of 5 years, which is less than 'age_min' [10 years]")
})


## chk_is_first_day_unit

test_that("'chk_is_first_day_unit' returns TRUE with valid dates", {
    expect_true(chk_is_first_day_unit(x = as.Date(c("2001-01-01", "2002-01-01")),
                                           name = "x",
                                           unit = "month"))
    expect_true(chk_is_first_day_unit(x = as.Date(c("2001-10-01", "2002-01-01")),
                                           name = "x",
                                           unit = "quarter"))
})

test_that("'chk_is_first_day_unit' returns expected message with invalid dates", {
    expect_identical(chk_is_first_day_unit(x = as.Date(c("2001-01-01", "2002-01-02")),
                                                name = "x",
                                                unit = "month"),
                     "element 2 [\"2002-01-02\"] of 'x' is not the first day of the month")
    expect_identical(chk_is_first_day_unit(x = as.Date(c("2001-01-01", "2001-02-01")),
                                                name = "x",
                                                unit = "quarter"),
                     "element 2 [\"2001-02-01\"] of 'x' is not the first day of the quarter")
})


## chk_is_first_day_unit_consec

test_that("'chk_is_first_day_unit_consec' returns TRUE with valid dates", {
    expect_true(chk_is_first_day_unit_consec(x = as.Date(c("2001-01-01", "2001-02-01")),
                                              name = "x",
                                              unit = "month"))
    expect_true(chk_is_first_day_unit_consec(x = as.Date(c("2001-01-01", "2001-04-01")),
                                              name = "x",
                                              unit = "quarter"))
    expect_true(chk_is_first_day_unit_consec(x = as.Date("2001-01-01"),
                                              name = "x",
                                              unit = "quarter"))
    expect_true(chk_is_first_day_unit_consec(x = as.Date(c("2000-10-1", "2001-01-01")),
                                              name = "x",
                                              unit = "quarter"))
})

test_that("'chk_is_first_day_unit_consec' returns expected message with invalid dates", {
    expect_identical(chk_is_first_day_unit_consec(x = as.Date(c("2001-01-01", "2001-03-01")),
                                                       name = "x",
                                                       unit = "month"),
                     "dates \"2001-01-01\" and \"2001-03-01\" in 'x' do not belong to consecutive months")
})


## chk_is_ge_scalar

test_that("'chk_is_ge_scalar' returns TRUE with inputs", {
    expect_true(chk_is_ge_scalar(x1 = 3, x2 = 2.9, name1 = "x1", name2 = "x2"))
    expect_true(chk_is_ge_scalar(x1 = as.Date("2001-10-01"), x2 = as.Date("2001-01-01"),
                                 name1 = "x1", name2 = "x2"))
    expect_true(chk_is_ge_scalar(x1 = NA, x2 = 2.9, name1 = "x1", name2 = "x2"))
})

test_that("'chk_is_ge_scalar' returns expected message with invalid inputs", {
    expect_identical(chk_is_ge_scalar(x1 = 2, x2 = 2.9, name1 = "x1", name2 = "x2"),
                     "'x1' [2] is less than 'x2' [2.9]")
    expect_identical(chk_is_ge_scalar(x1 = as.Date("2000-10-01"), x2 = as.Date("2001-01-01"),
                                      name1 = "x1", name2 = "x2"),
                     "'x1' [2000-10-01] is less than 'x2' [2001-01-01]")
})


## chk_is_ge_vector

test_that("'chk_is_ge_vector' returns TRUE with inputs", {
    expect_true(chk_is_ge_vector(x1 = 3:5, x2 = c(2.9, 4, 5), name1 = "x1", name2 = "x2"))
    expect_true(chk_is_ge_vector(x1 = as.Date(c("2001-10-01", "2000-03-03")),
                                 x2 = as.Date(c("2001-01-01", "2000-03-03")),
                                 name1 = "x1", name2 = "x2"))
    expect_true(chk_is_ge_vector(x1 = as.Date(c("2001-10-01", NA)),
                                 x2 = as.Date(c("2001-01-01", "2000-03-03")),
                                 name1 = "x1", name2 = "x2"))
})

test_that("'chk_is_ge_vector' returns expected message with invalid inputs", {
    expect_identical(chk_is_ge_vector(x1 = c(2, 3), x2 = c(1, 4),
                                      name1 = "x1", name2 = "x2"),
                     "element 2 of 'x1' [3] is less than element 2 of 'x2' [4]")
    expect_identical(chk_is_ge_vector(x1 = as.Date("2000-10-01"), x2 = as.Date("2001-01-01"),
                                      name1 = "x1", name2 = "x2"),
                     "element 1 of 'x1' [2000-10-01] is less than element 1 of 'x2' [2001-01-01]")
})


## chk_is_gt_scalar

test_that("'chk_is_gt_scalar' returns TRUE with inputs", {
    expect_true(chk_is_gt_scalar(x1 = 3, x2 = 2.9, name1 = "x1", name2 = "x2"))
    expect_true(chk_is_gt_scalar(x1 = as.Date("2001-10-01"), x2 = as.Date("2001-01-01"),
                                 name1 = "x1", name2 = "x2"))
    expect_true(chk_is_gt_scalar(x1 = NA, x2 = 2.9, name1 = "x1", name2 = "x2"))
})

test_that("'chk_is_gt_scalar' returns expected message with invalid inputs", {
    expect_identical(chk_is_gt_scalar(x1 = 2, x2 = 2.9, name1 = "x1", name2 = "x2"),
                     "'x1' [2] is less than or equal to 'x2' [2.9]")
    expect_identical(chk_is_gt_scalar(x1 = as.Date("2000-10-01"), x2 = as.Date("2001-01-01"),
                                      name1 = "x1", name2 = "x2"),
                     "'x1' [2000-10-01] is less than or equal to 'x2' [2001-01-01]")
    expect_identical(chk_is_gt_scalar(x1 = as.Date("2000-10-01"), x2 = as.Date("2000-10-01"),
                                      name1 = "x1", name2 = "x2"),
                     "'x1' [2000-10-01] is less than or equal to 'x2' [2000-10-01]")
})


## chk_is_gt_vector

test_that("'chk_is_gt_vector' returns TRUE with inputs", {
    expect_true(chk_is_gt_vector(x1 = 3:5, x2 = c(2.9, 3.5, 4.99999999), name1 = "x1", name2 = "x2"))
    expect_true(chk_is_gt_vector(x1 = as.Date(c("2001-10-01", "2000-03-03")),
                                 x2 = as.Date(c("2001-01-01", "2000-03-02")),
                                 name1 = "x1", name2 = "x2"))
    expect_true(chk_is_gt_vector(x1 = as.Date(c("2001-10-01", NA)),
                                 x2 = as.Date(c("2001-01-01", "2000-03-03")),
                                 name1 = "x1", name2 = "x2"))
})

test_that("'chk_is_gt_vector' returns expected message with invalid inputs", {
    expect_identical(chk_is_gt_vector(x1 = c(2, 3), x2 = c(1, 4),
                                      name1 = "x1", name2 = "x2"),
                     "element 2 of 'x1' [3] is less than or equal to element 2 of 'x2' [4]")
    expect_identical(chk_is_gt_vector(x1 = as.Date("2000-10-01"), x2 = as.Date("2000-10-01"),
                                      name1 = "x1", name2 = "x2"),
                     "element 1 of 'x1' [2000-10-01] is less than or equal to element 1 of 'x2' [2000-10-01]")
})


## chk_is_integer_consec

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


## chk_is_logical_flag

test_that("'chk_is_logical_flag' returns TRUE with valid logical_flag", {
    expect_true(chk_is_logical_flag(x = TRUE,
                                    name = "x"))
    expect_true(chk_is_logical_flag(x = FALSE,
                                    name = "x"))
})

test_that("'chk_is_logical_flag' returns expected message with invalid argument", {
    expect_identical(chk_is_logical_flag(x = 1,
                                   name = "x"),
                     "'x' does not have type \"logical\"")
    expect_identical(chk_is_logical_flag(x = c(TRUE, FALSE),
                                   name = "x"),
                     "'x' does not have length 1")
    expect_identical(chk_is_logical_flag(x = NA,
                                   name = "x"),
                     "'x' is NA")
})

## chk_is_multiple_of

test_that("'chk_is_multiple_of' returns TRUE with valid inputs", {
    expect_true(chk_is_multiple_of(x1 = 30L,
                                   x2 = 5L,
                                   name1 = "x1",
                                   name2 = "x2"))
    expect_true(chk_is_multiple_of(x1 = Inf,
                                   x2 = 5L,
                                   name1 = "x1",
                                   name2 = "x2",
                                   inf_ok = TRUE))
})

test_that("'chk_is_multiple_of' returns expected message with invalid argument", {
    expect_identical(chk_is_multiple_of(x1 = Inf,
                                        x2 = 5L,
                                        name1 = "x1",
                                        name2 = "x2"),
                     "'x1' is non-finite")
    expect_identical(chk_is_multiple_of(x1 = 31L,
                                        x2 = 5L,
                                        name1 = "x1",
                                        name2 = "x2"),
                     "'x1' [31] is not a multiple of 'x2' [5]")
})


## chk_is_non_negative_scalar

test_that("'chk_is_non_negative_scalar' returns TRUE with valid input", {
    expect_true(chk_is_non_negative_scalar(x = 1L,
                                           name = "x"))
    expect_true(chk_is_non_negative_scalar(x = 0,
                                           name = "x"))
})

test_that("'chk_is_non_negative_scalar' returns expected message with invalid argument", {
    expect_identical(chk_is_non_negative_scalar(x = NA_integer_,
                                            name = "x"),
                     "'x' is NA")
    expect_identical(chk_is_non_negative_scalar(x = c(1, 1),
                                   name = "x"),
                     "'x' does not have length 1")
    expect_identical(chk_is_non_negative_scalar(x = "1",
                                            name = "x"),
                     "'x' does not have type \"numeric\"")
    expect_identical(chk_is_non_negative_scalar(x = -0.001,
                                                name = "x"),
                     "'x' [-0.001] is negative")
})


## chk_is_non_negative_vector

test_that("'chk_is_non_negative_vector' returns TRUE with valid input", {
    expect_true(chk_is_non_negative_vector(x = c(0, 0.001),
                                           name = "x"))
    expect_true(chk_is_non_negative_vector(x = c(0.001, 0, Inf),
                                       name = "x"))
})

test_that("'chk_is_non_negative_vector' returns expected message with invalid argument", {
    expect_identical(chk_is_non_negative_vector(x = c(NA_integer_, 1L),
                                            name = "x"),
                     "'x' has NAs")
    expect_identical(chk_is_non_negative_vector(x = c("1", "2"),
                                            name = "x"),
                     "'x' does not have type \"numeric\"")
    expect_identical(chk_is_non_negative_vector(x = c(0.1, -0.1),
                                            name = "x"),
                     "element 2 of 'x' [-0.1] is negative")
})


## chk_is_positive_scalar

test_that("'chk_is_positive_scalar' returns TRUE with valid input", {
    expect_true(chk_is_positive_scalar(x = 1L,
                                       name = "x"))
    expect_true(chk_is_positive_scalar(x = 0.001,
                                       name = "x"))
})

test_that("'chk_is_positive_scalar' returns expected message with invalid argument", {
    expect_identical(chk_is_positive_scalar(x = NA_integer_,
                                            name = "x"),
                     "'x' is NA")
    expect_identical(chk_is_positive_scalar(x = c(1, 1),
                                   name = "x"),
                     "'x' does not have length 1")
    expect_identical(chk_is_positive_scalar(x = "1",
                                            name = "x"),
                     "'x' does not have type \"numeric\"")
    expect_identical(chk_is_positive_scalar(x = 0,
                                            name = "x"),
                     "'x' [0] is non-positive")
})


## chk_is_positive_vector

test_that("'chk_is_positive_vector' returns TRUE with valid input", {
    expect_true(chk_is_positive_vector(x = c(1, 0.001),
                                       name = "x"))
    expect_true(chk_is_positive_vector(x = c(0.001, Inf),
                                       name = "x"))
})

test_that("'chk_is_positive_vector' returns expected message with invalid argument", {
    expect_identical(chk_is_positive_vector(x = c(NA_integer_, 1L),
                                            name = "x"),
                     "'x' has NAs")
    expect_identical(chk_is_positive_vector(x = c("1", "2"),
                                            name = "x"),
                     "'x' does not have type \"numeric\"")
    expect_identical(chk_is_positive_vector(x = c(0.1, 0),
                                            name = "x"),
                     "element 2 of 'x' [0] is non-positive")
})


## chk_is_string

test_that("'chk_is_string' returns TRUE with valid string", {
    expect_true(chk_is_string(x = "a",
                              name = "x"))
    expect_true(chk_is_string(x = "",
                              name = "x"))
    expect_true(chk_is_string(x = "abc",
                              name = "x"))
})

test_that("'chk_is_string' returns expected message with invalid argument", {
    expect_identical(chk_is_string(x = 1,
                                   name = "x"),
                     "'x' does not have type \"character\"")
    expect_identical(chk_is_string(x = c("a", "b"),
                                   name = "x"),
                     "'x' does not have length 1")
    expect_identical(chk_is_string(x = NA_character_,
                                   name = "x"),
                     "'x' is NA")
})


## chk_is_strictly_increasing

test_that("'chk_is_strictly_increasing' returns TRUE with valid vector", {
    expect_true(chk_is_strictly_increasing(x = 1,
                              name = "x"))
    expect_true(chk_is_strictly_increasing(x = 1:3,
                              name = "x"))
    expect_true(chk_is_strictly_increasing(x = c(-Inf, 0, Inf),
                              name = "x"))
})

test_that("'chk_is_strictly_increasing' returns expected message with invalid argument", {
    expect_identical(chk_is_strictly_increasing(x = "a",
                                                name = "x"),
                     "'x' does not have type \"numeric\"")
    expect_identical(chk_is_strictly_increasing(x = NA_integer_,
                                                name = "x"),
                     "'x' has NAs")
    expect_identical(chk_is_strictly_increasing(x = c(1, 3, 2),
                                                name = "x"),
                     "'x' is not strictly increasing : element 2 [3] is greater than or equal to element 3 [2]")
})


## chk_length_same

test_that("'chk_length_same' returns TRUE with valid vector", {
    expect_true(chk_length_same(x1 = 1:3,
                                x2 = 3:1,
                                name1 = "x1",
                                name2 = "x2"))
    expect_true(chk_length_same(x1 = character(),
                                x2 = integer(),
                                name1 = "x1",
                                name2 = "x2"))
})

test_that("'chk_length_same' returns expected message with invalid argument", {
    expect_identical(chk_length_same(x1 = integer(),
                                     x2 = 3L,
                                     name1 = "x1",
                                     name2 = "x2"),
                     "length of 'x1' [0] not equal to length of 'x2' [1]")
    expect_identical(chk_length_same(x1 = 1L,
                                     x2 = c("a", "b"),
                                     name1 = "x1",
                                     name2 = "x2"),
                     "length of 'x1' [1] not equal to length of 'x2' [2]")
})


## chk_length_same_or_1

test_that("'chk_length_same_or_1' returns TRUE with valid vector", {
    expect_true(chk_length_same_or_1(x1 = 1:3,
                                     x2 = 3:1,
                                     name1 = "x1",
                                     name2 = "x2"))
    expect_true(chk_length_same_or_1(x1 = 1L,
                                     x2 = 3:1,
                                     name1 = "x1",
                                     name2 = "x2"))
    expect_true(chk_length_same_or_1(x1 = 1:3,
                                     x2 = 3L,
                                     name1 = "x1",
                                     name2 = "x2"))
    expect_true(chk_length_same_or_1(x1 = 1L,
                                     x2 = 3L,
                                     name1 = "x1",
                                     name2 = "x2"))
})

test_that("'chk_length_same_or_1' returns expected message with invalid argument", {
    expect_identical(chk_length_same_or_1(x1 = integer(),
                                          x2 = 3L,
                                          name1 = "x1",
                                          name2 = "x2"),
                     "'x1' has length 0")
    expect_identical(chk_length_same_or_1(x1 = 1L,
                                          x2 = integer(),
                                          name1 = "x1",
                                          name2 = "x2"),
                     "'x2' has length 0")
    expect_identical(chk_length_same_or_1(x1 = 1:2,
                                          x2 = 3:1,
                                          name1 = "x1",
                                          name2 = "x2"),
                     "'x1' has length 2 and 'x2' has length 3 : should have same lengths, or one should have length 1")
})


## chk_lt_age_max

test_that("'chk_lt_age_max' returns TRUE with valid inputs", {
    expect_true(chk_lt_age_max(age = 5L,
                               age_max = 10L,
                               date = as.Date("2005-01-02"),
                               dob = as.Date("2000-01-01"),
                               unit = "year"))
})

test_that("'chk_lt_age_max' returns expected message with invalid dates", {
    expect_identical(chk_lt_age_max(age = 5L,
                                    age_max = 5L,
                                    date = as.Date("2005-01-02"),
                                    dob = as.Date("2000-01-01"),
                                    unit = "year"),
                     "'date' [\"2005-01-02\"] and 'dob' [\"2000-01-01\"] imply an age of 5 years, which is greater than or equal to 'age_max' [5 years]")
})

    
## chk_names_complete

test_that("'chk_names_complete' returns TRUE with valid array", {
    x <- c(A = 1, B = 2)
    expect_true(chk_names_complete(x = x,
                                   name = "x"))
    x <- c()
    expect_true(chk_names_complete(x = x,
                                   name = "x"))
})

test_that("'chk_names_complete' returns expected message with invalid argument", {
    x <- c(A = 1, B = 2)
    x_wrong <- x
    names(x_wrong)[[1]] <- NA
    expect_identical(chk_names_complete(x = x_wrong,
                                        name = "x"),
                     "names for 'x' have NAs")
    x_wrong <- x
    names(x_wrong)[[1]] <- ""
    expect_identical(chk_names_complete(x = x_wrong,
                                                 name = "x"),
                     "names for 'x' have blanks")
    x_wrong <- x
    names(x_wrong)[[2]] <- "A"
    expect_identical(chk_names_complete(x = x_wrong,
                                        name = "x"),
                     "names for 'x' have duplicate [\"A\"]")
})


## chk_names_dimnames_complete

test_that("'chk_names_dimnames_complete' returns TRUE with valid array", {
    x <- array(0L,
               dim = 2:3,
               dimnames = list(A = 1:2, b = 1:3))
    expect_true(chk_names_dimnames_complete(x = x,
                                            name = "x"))
    x <- array(0L,
               dim = c(0, 3),
               dimnames = list(A = character(), b = 1:3))
    expect_true(chk_names_dimnames_complete(x = x,
                                            name = "x"))
})

test_that("'chk_names_dimnames_complete' returns expected message with invalid argument", {
    x <- array(0L,
               dim = 2:3,
               dimnames = list(A = 1:2, b = 1:3))
    x_wrong <- x
    names(dimnames(x_wrong))[[1]] <- NA
    expect_identical(chk_names_dimnames_complete(x = x_wrong,
                                                 name = "x"),
                     "names for dimnames of 'x' have NAs")
    x_wrong <- x
    names(dimnames(x_wrong))[[1]] <- ""
    expect_identical(chk_names_dimnames_complete(x = x_wrong,
                                                 name = "x"),
                     "names for dimnames of 'x' have blanks")
    x_wrong <- x
    names(dimnames(x_wrong))[[2]] <- "A"
    expect_identical(chk_names_dimnames_complete(x = x_wrong,
                                                 name = "x"),
                     "names for dimnames of 'x' have duplicate [\"A\"]")
})


## chk_trans_list

test_that("'chk_trans_list' returns TRUE with valid input", {
    expect_true(chk_trans_list(x = list(a = c("b", "c"),
                                        b = "c",
                                        c = NULL),
                               name = "x"))
    expect_true(chk_trans_list(x = list(a = c("b", "c", "a"),
                                        b = "c",
                                        c = NULL,
                                        d = c("b", "a")),
                               name = "x"))
})

test_that("'chk_trans_list' returns expected message with invalid argument", {
    x_wrong = list(a = c("b", "c"),
                   b = "c",
                   c = as.character(NA))
    expect_identical(chk_trans_list(x = x_wrong,
                                    name = "x_wrong"),
                     "element \"c\" of 'x_wrong' has NAs")
    x_wrong = list(a = c("b", "c"),
                   b = "c",
                   c = "")
    expect_identical(chk_trans_list(x = x_wrong,
                                    name = "x_wrong"),
                     "element \"c\" of 'x_wrong' has blanks")
    x_wrong = list(a = c("b", "c"),
                   b = "c",
                   c = c("a", "a"))
    expect_identical(chk_trans_list(x = x_wrong,
                                    name = "x_wrong"),
                     "element \"c\" of 'x_wrong' has duplicates")
    x_wrong = list(a = c("b", "c"),
                   b = "wrong",
                   c = NULL)
    expect_identical(chk_trans_list(x = x_wrong,
                                    name = "x_wrong"),
                     paste("value \"wrong\" in element \"b\" of 'x_wrong' invalid :",
                           "\"wrong\" is not the name of an element of 'x_wrong'"))
    x_wrong = list(a = c("b", "c"),
                   b = 1L,
                   c = NULL)
    expect_identical(chk_trans_list(x = x_wrong,
                                    name = "x_wrong"),
                     "element \"b\" of 'x_wrong' has class \"integer\"")
})

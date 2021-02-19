
context("chk-composite")

## chk_all_x1_in_x2 -----------------------------------------------------------

test_that("'chk_all_x1_in_x2' returns TRUE with valid inputs", {
    expect_true(chk_all_x1_in_x2(x1 = 1:3, x2 = 1:5,
                                 name1 = "x1", name2 = "x2",
                                 exclude_zero = FALSE))
    expect_true(chk_all_x1_in_x2(x1 = 1:3, x2 = 1:5,
                                 name1 = "x1", name2 = "x2",
                                 exclude_zero = TRUE))
    expect_true(chk_all_x1_in_x2(x1 = c(0L, 1:3, 0L), x2 = 1:5,
                                 name1 = "x1", name2 = "x2",
                                 exclude_zero = TRUE))
    expect_true(chk_all_x1_in_x2(x1 = 0L, x2 = 1:5,
                                 name1 = "x1", name2 = "x2",
                                 exclude_zero = TRUE))
    expect_true(chk_all_x1_in_x2(x1 = integer(), x2 = 1:5,
                                 name1 = "x1", name2 = "x2",
                                 exclude_zero = TRUE))
})

test_that("'chk_all_x1_in_x2' returns expected message with invalid inputs", {
    expect_identical(chk_all_x1_in_x2(x1 = 1:3, x2 = 1:2,
                                      name1 = "x1", name2 = "x2",
                                      exclude_zero = FALSE),
                     "element from x1 not found in x2 : 3")
    expect_identical(chk_all_x1_in_x2(x1 = 0L, x2 = 1:2,
                                      name1 = "x1", name2 = "x2",
                                      exclude_zero = FALSE),
                     "element from x1 not found in x2 : 0")
})


## chk_array_metadata_complete ------------------------------------------------

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


## chk_character_complete -----------------------------------------------------

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


## chk_dim_min_length -----------------------------------------------------

test_that("'chk_dim_min_length' returns TRUE with valid character vector", {
    expect_true(chk_dim_min_length(length_actual = 4L,
                                   length_min = 4L,
                                   name = "time"))
})

test_that("'chk_dim_min_length' returns expected message with invalid character vector", {
    expect_identical(chk_dim_min_length(length_actual = 2L,
                                        length_min = 3L,
                                        name = "time"),
                     "length of time dimension [2] less than minimum [3]")
})


## chk_omitted --------------------------------------------------------------

test_that("'chk_omitted' returns TRUE with valid inputs", {
    expect_true(chk_omitted(index = 1:2,
                            map_dim = c(0L, 0L, 1L, 2L, 3L),
                            name_index = "indices_orig_self",
                            name_dim = "origin"))
    expect_true(chk_omitted(index = integer(),
                            map_dim = c(0L, 0L, 1L, 2L, 3L),
                            name_index = "indices_orig_self",
                            name_dim = "origin"))
})

test_that("'chk_omitted' returns expected message with invalid argument", {
    expect_identical(chk_omitted(index = 1:2,
                                 map_dim = c(0L, 4L, 1L, 2L, 3L),
                                 name_index = "indices_orig_self",
                                 name_dim = "origin"),
                     paste("origin dimension identified by 'indices_orig_self' is not omitted,",
                           "according to 'map_dim'"))
})


## chk_not_omitted --------------------------------------------------------------

test_that("'chk_not_omitted' returns TRUE with valid inputs", {
    expect_true(chk_not_omitted(index = 3:4,
                                map_dim = c(0L, 0L, 1L, 2L, 3L),
                                name_index = "indices_dest_self",
                                name_dim = "destination"))
    expect_true(chk_not_omitted(index = integer(),
                                map_dim = c(0L, 0L, 1L, 2L, 3L),
                                name_index = "indices_dest_self",
                                name_dim = "destination"))
})

test_that("'chk_not_omitted' returns expected message with invalid argument", {
    expect_identical(chk_not_omitted(index = 1:2,
                                     map_dim = c(0L, 0L, 1L, 0L, 2L),
                                     name_index = "indices_dest_self",
                                     name_dim = "destination"),
                     paste("destination dimension identified by 'indices_dest_self' is omitted,",
                           "according to 'map_dim'"))
})



## chk_dimnames_complete ------------------------------------------------------

test_that("'chk_dimnames_complete' returns TRUE with valid array", {
    x <- array(0L,
               dim = 2:3,
               dimnames = list(A = 1:2, b = 1:3))
    expect_true(chk_dimnames_complete(x = x,
                                      name = "x"))
    x <- array(0L,
               dim = 2:3,
               dimnames = list(A = 1:2, b = c(NA, 2:3)))
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
               dimnames = list(A = 1:2, B = 1:3))
    x_wrong <- x
    dimnames(x_wrong)[1] <- list(NULL)
    expect_identical(chk_dimnames_complete(x = x_wrong,
                                                 name = "x"),
                     "\"A\" dimension of 'x' does not have dimnames")
    x_wrong <- x
    dimnames(x_wrong)[[2]][1:2] <- NA
    expect_identical(chk_dimnames_complete(x = x_wrong,
                                           name = "x"),
                     "dimnames for \"B\" dimension of 'x' have 2 NAs")
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


## chk_dimtypes_mutually_compatible -------------------------------------------

test_that("'chk_dimtypes_mutually_compatible' returns TRUE with valid input", {
    expect_true(chk_dimtypes_mutually_compatible(dimtypes = c("age",
                                                              "time",
                                                              "state",
                                                              "state",
                                                              "iteration")))
    expect_true(chk_dimtypes_mutually_compatible(dimtypes = character()))
})

test_that("'chk_dimtypes_mutually_compatible' returns expected message with invalid input", {
    expect_identical(chk_dimtypes_mutually_compatible(dimtypes = c("age",
                                                                   "time",
                                                                   "state",
                                                                   "age",
                                                                   "iteration")),
                     "two dimensions with dimtype \"age\"")
    expect_identical(chk_dimtypes_mutually_compatible(dimtypes = c("age",
                                                                   "time",
                                                                   "state",
                                                                   "quantile",
                                                                   "iteration")),
                     "dimension with dimtype \"iteration\" and dimension with dimtype \"quantile\"")
})


## chk_names_pairs_complete ------------------------------------------------

test_that("'chk_integer_in_range' returns TRUE with valid input", {
    expect_true(chk_integer_in_range(x = c(1:5, NA),
                                     min = 1L,
                                     max = 5L,
                                     name = "x"))
    expect_true(chk_integer_in_range(x = NA_integer_,
                                     min = 1L,
                                     max = 5L,
                                     name = "x"))
    expect_true(chk_integer_in_range(x = integer(),
                                     min = 1L,
                                     max = 5L,
                                     name = "x"))
})
                
test_that("'chk_integer_in_range' returns expected message with invalid input", {
    expect_identical(chk_integer_in_range(x = c(1:5, NA, 0L),
                                          min = 1L,
                                          max = 5L,
                                          name = "x"),
                     "element of 'x' [0] is less than 1")
    expect_identical(chk_integer_in_range(x = c(1:5, NA, 6L),
                                          min = 1L,
                                          max = 5L,
                                          name = "x"),
                     "element of 'x' [6] is greater than 5")
})


## chk_names_pairs_complete ------------------------------------------------

test_that("'chk_names_pairs_complete' returns TRUE with valid input", {
    expect_true(chk_names_pairs_complete(names = c("age",
                                                   "reg_orig",
                                                   "eth_child",
                                                   "reg_dest",
                                                   "eth_parent")))
    expect_true(chk_names_pairs_complete(names = character()))
})

test_that("'chk_names_pairs_complete' returns expected message with invalid input", {
    expect_identical(chk_names_pairs_complete(names = c("age",
                                                        "reg_orig",
                                                        "eth",
                                                        "reg_dest",
                                                        "eth_parent")),
                     paste("dimension \"eth_parent\" with dimtype \"parent\" does not have",
                           "paired dimension \"eth_child\" with dimtype \"child\""))
})


## chk_names_pairs_suffix --------------------------------------------------

test_that("'chk_names_pairs_suffix' returns TRUE with valid input", {
    expect_true(chk_names_pairs_suffix(dimtypes = c("age",
                                                    "origin",
                                                    "child",
                                                    "destination",
                                                    "parent"),
                                       names = c("age",
                                                 "reg_orig",
                                                 "eth_child",
                                                 "reg_dest",
                                                 "eth_parent")))
    expect_true(chk_names_pairs_suffix(dimtypes = character(),
                                       names = character()))
})

test_that("'chk_names_pairs_suffix' returns expected message with invalid input", {
    expect_identical(chk_names_pairs_suffix(dimtypes = c("age",
                                                         "origin",
                                                         "child",
                                                         "destination",
                                                         "parent"),
                                            names = c("age",
                                                      "reg_orig",
                                                      "eth",
                                                      "reg_dest",
                                                      "eth_parent")),
                     "dimension \"eth\" has dimtype \"child\" but name does not end with \"_child\"")
    expect_identical(chk_names_pairs_suffix(dimtypes = c("age",
                                                         "origin",
                                                         "state",
                                                         "destination"),
                                            names = c("age",
                                                      "reg_orig",
                                                      "eth_child",
                                                      "reg_dest")),
                     "dimension \"eth_child\" has name ending with \"_child\" but does not have dimtype \"child\"")
})


## chk_ge_break_min_age -------------------------------------------------------

test_that("'chk_ge_break_min_age' returns TRUE with valid inputs", {
    expect_true(chk_ge_break_min_age(age = 5L,
                                     break_min = 0L,
                                     date = as.Date("2005-01-02"),
                                     dob = as.Date("2000-01-01"),
                                     unit = "year"))
})

test_that("'chk_ge_break_min_age' returns expected message with invalid dates", {
    expect_identical(chk_ge_break_min_age(age = 5L,
                                          break_min = 10L,
                                          date = as.Date("2005-01-02"),
                                          dob = as.Date("2000-01-01"),
                                          unit = "year"),
                     "'date' [\"2005-01-02\"] and 'dob' [\"2000-01-01\"] imply an age of 5 years, which is less than 'break_min' [10 years]")
})


## chk_ge_break_min_date ------------------------------------------------------

test_that("'chk_ge_break_min_date' returns TRUE with valid inputs", {
    expect_true(chk_ge_break_min_date(date = as.Date(c("2001-03-02", "2005-12-01", NA)),
                                      break_min = as.Date("2000-01-01")))
    expect_true(chk_ge_break_min_date(date = as.Date(c("2001-03-02", "2005-12-01", NA)),
                                      break_min = NULL))
})

test_that("'chk_ge_break_min_date' returns expected message with invalid dates", {
    expect_identical(chk_ge_break_min_date(date = as.Date(c("2001-03-02", "1995-12-01", NA)),
                                           break_min = "2000-01-01"),
                     "'date' has value [\"1995-12-01\"] that is less than 'break_min' [\"2000-01-01\"]")
})


## chk_is_class_obj -----------------------------------------------------------

test_that("'chk_is_class_obj' returns TRUE with valid inputs", {
    expect_true(chk_is_class_obj(x = "a",
                                    name = "x",
                                    class = "character"))
})

test_that("'chk_is_class_obj' returns expected message with invalid input", {
    expect_identical(chk_is_class_obj(x = 1:2,
                                      name = "x",
                                      class = "character"),
                     "'x' has class \"integer\" : should instead inherit from class \"character\"")                
})


## chk_is_class_list ----------------------------------------------------------

test_that("'chk_is_class_list' returns TRUE with valid inputs", {
    expect_true(chk_is_class_list(x = list("a", "b"),
                                  name = "x",
                                  class = "character"))
    expect_true(chk_is_class_list(x = list(),
                                  name = "x",
                                  class = "character"))
})

test_that("'chk_is_class_list' returns expected message with invalid input", {
    expect_identical(chk_is_class_list(x = list("a", 1L),
                                       name = "x",
                                       class = "character"),
                     "element 2 of 'x' has class \"integer\" : should instead inherit from class \"character\"")                
})


## chk_first_day_unit_scalar --------------------------------------------------

test_that("'chk_first_day_unit_scalar' returns TRUE with valid dates", {
    expect_true(chk_first_day_unit_scalar(x = "2001-01-01",
                                             name = "x",
                                             unit = "month"))
    expect_true(chk_first_day_unit_scalar(x = "2001-10-01",
                                             name = "x",
                                             unit = "quarter"))
    expect_true(chk_first_day_unit_scalar(x = "2001-10-01",
                                             name = "x",
                                             unit = "year"))
})

test_that("'chk_first_day_unit_scalar' returns expected message with invalid dates", {
    expect_identical(chk_first_day_unit_scalar(x = "2002-01-02",
                                                  name = "x",
                                                  unit = "month"),
                     "'x' [\"2002-01-02\"] is not the first day of the month")
    expect_identical(chk_first_day_unit_scalar(x = "2001-02-01",
                                                  name = "x",
                                                  unit = "quarter"),
                     "'x' [\"2001-02-01\"] is not the first day of the quarter")
    expect_identical(chk_first_day_unit_scalar(x = "2001-01-02",
                                                  name = "x",
                                                  unit = "year"),
                     paste("'x' [\"2001-01-02\"] is not the first day of the year",
                           "(years assumed to start on the first day of January)"))
})


## chk_first_day_unit_vector --------------------------------------------------

test_that("'chk_first_day_unit_vector' returns TRUE with valid dates", {
    expect_true(chk_first_day_unit_vector(x = as.Date(c("2001-01-01", "2002-01-01")),
                                           name = "x",
                                           unit = "month"))
    expect_true(chk_first_day_unit_vector(x = as.Date(c("2001-10-01", "2002-01-01")),
                                           name = "x",
                                           unit = "quarter"))
    expect_true(chk_first_day_unit_vector(x = as.Date(c("2001-10-01", "2002-10-01")),
                                      name = "x",
                                      unit = "year"))
    expect_true(chk_first_day_unit_vector(x = as.Date(c("2001-10-01", "2010-10-01")),
                                      name = "x",
                                      unit = "year"))
})

test_that("'chk_first_day_unit_vector' returns expected message with invalid dates", {
    expect_identical(chk_first_day_unit_vector(x = as.Date(c("2001-01-01", "2002-01-02")),
                                                name = "x",
                                                unit = "month"),
                     "element 2 [\"2002-01-02\"] of 'x' is not the first day of the month")
    expect_identical(chk_first_day_unit_vector(x = as.Date(c("2001-01-01", "2001-02-01")),
                                                name = "x",
                                                unit = "quarter"),
                     "element 2 [\"2001-02-01\"] of 'x' is not the first day of the quarter")
    expect_identical(chk_first_day_unit_vector(x = as.Date(c("2001-01-01", "2001-02-01")),
                                           name = "x",
                                           unit = "year"),
                     paste("element 2 [\"2001-02-01\"] of 'x' is not the first day of the year",
                           "(years assumed to start on the first day of January)"))
    expect_identical(chk_first_day_unit_vector(x = as.Date(c("2001-01-01", "2005-01-02")),
                                           name = "x",
                                           unit = "year"),
                     paste("element 2 [\"2005-01-02\"] of 'x' is not the first day of the year",
                           "(years assumed to start on the first day of January)"))
})


## chk_first_day_unit_consec --------------------------------------------------

test_that("'chk_first_day_unit_consec' returns TRUE with valid dates", {
    expect_true(chk_first_day_unit_consec(x = as.Date(c("2001-01-01", "2001-02-01")),
                                             name = "x",
                                             unit = "month"))
    expect_true(chk_first_day_unit_consec(x = as.Date(c("2001-01-01", "2001-04-01")),
                                             name = "x",
                                             unit = "quarter"))
    expect_true(chk_first_day_unit_consec(x = as.Date("2001-01-01"),
                                             name = "x",
                                             unit = "quarter"))
    expect_true(chk_first_day_unit_consec(x = as.Date(c("2000-10-1", "2001-01-01")),
                                             name = "x",
                                             unit = "quarter"))
})

test_that("'chk_first_day_unit_consec' returns expected message with invalid dates", {
    expect_identical(chk_first_day_unit_consec(x = as.Date(c("2001-01-01", "2001-03-01")),
                                               name = "x",
                                               unit = "month"),
                     "dates \"2001-01-01\" and \"2001-03-01\" in 'x' do not belong to consecutive months")
})


## chk_ge_scalar --------------------------------------------------------------

test_that("'chk_ge_scalar' returns TRUE with valid inputs", {
    expect_true(chk_ge_scalar(x1 = 3, x2 = 2.9, name1 = "x1", name2 = "x2"))
    expect_true(chk_ge_scalar(x1 = as.Date("2001-10-01"), x2 = as.Date("2001-01-01"),
                                 name1 = "x1", name2 = "x2"))
    expect_true(chk_ge_scalar(x1 = NA, x2 = 2.9, name1 = "x1", name2 = "x2"))
})

test_that("'chk_ge_scalar' returns expected message with invalid inputs", {
    expect_identical(chk_ge_scalar(x1 = 2, x2 = 2.9, name1 = "x1", name2 = "x2"),
                     "'x1' [2] is less than 'x2' [2.9]")
    expect_identical(chk_ge_scalar(x1 = as.Date("2000-10-01"), x2 = as.Date("2001-01-01"),
                                      name1 = "x1", name2 = "x2"),
                     "'x1' [2000-10-01] is less than 'x2' [2001-01-01]")
})


## chk_ge_vector --------------------------------------------------------------

test_that("'chk_ge_vector' returns TRUE with valid inputs", {
    expect_true(chk_ge_vector(x1 = 3:5, x2 = c(2.9, 4, 5), name1 = "x1", name2 = "x2"))
    expect_true(chk_ge_vector(x1 = 3:5, x2 = 2.9, name1 = "x1", name2 = "x2"))
    expect_true(chk_ge_vector(x1 = as.Date(c("2001-10-01", "2000-03-03")),
                                 x2 = as.Date(c("2001-01-01", "2000-03-03")),
                                 name1 = "x1", name2 = "x2"))
    expect_true(chk_ge_vector(x1 = as.Date(c("2001-10-01", NA)),
                                 x2 = as.Date(c("2001-01-01", "2000-03-03")),
                                 name1 = "x1", name2 = "x2"))
})

test_that("'chk_ge_vector' returns expected message with invalid inputs", {
    expect_identical(chk_ge_vector(x1 = c(2, 3), x2 = c(1, 4),
                                      name1 = "x1", name2 = "x2"),
                     "element 2 of 'x1' [3] is less than element 2 of 'x2' [4]")
    expect_identical(chk_ge_vector(x1 = as.Date(c("2000-10-01", "2020-01-01")),
                                   x2 = as.Date(c("2001-01-01", "2001-01-01")),
                                   name1 = "x1", name2 = "x2"),
                     "element 1 of 'x1' [2000-10-01] is less than element 1 of 'x2' [2001-01-01]")
    expect_identical(chk_ge_vector(x1 = as.Date(c("2000-10-01", "2020-01-01")),
                                   x2 = as.Date("2001-01-01"),
                                   name1 = "x1", name2 = "x2"),
                     "element 1 of 'x1' [2000-10-01] is less than 'x2' [2001-01-01]")
    expect_identical(chk_ge_vector(x1 = c(2, 3, 10), x2 = c(1, 4),
                                      name1 = "x1", name2 = "x2"),
                     "'x1' has length 3 and 'x2' has length 2")
})


## chk_gt_scalar --------------------------------------------------------------

test_that("'chk_gt_scalar' returns TRUE with valid inputs", {
    expect_true(chk_gt_scalar(x1 = 3, x2 = 2.9, name1 = "x1", name2 = "x2"))
    expect_true(chk_gt_scalar(x1 = as.Date("2001-10-01"), x2 = as.Date("2001-01-01"),
                                 name1 = "x1", name2 = "x2"))
    expect_true(chk_gt_scalar(x1 = NA, x2 = 2.9, name1 = "x1", name2 = "x2"))
})

test_that("'chk_gt_scalar' returns expected message with invalid inputs", {
    expect_identical(chk_gt_scalar(x1 = 2, x2 = 2.9, name1 = "x1", name2 = "x2"),
                     "'x1' [2] is less than or equal to 'x2' [2.9]")
    expect_identical(chk_gt_scalar(x1 = as.Date("2000-10-01"), x2 = as.Date("2001-01-01"),
                                      name1 = "x1", name2 = "x2"),
                     "'x1' [2000-10-01] is less than or equal to 'x2' [2001-01-01]")
    expect_identical(chk_gt_scalar(x1 = as.Date("2000-10-01"), x2 = as.Date("2000-10-01"),
                                      name1 = "x1", name2 = "x2"),
                     "'x1' [2000-10-01] is less than or equal to 'x2' [2000-10-01]")
})


## chk_gt_vector --------------------------------------------------------------

test_that("'chk_gt_vector' returns TRUE with valid inputs", {
    expect_true(chk_gt_vector(x1 = 3:5, x2 = c(2.9, 3.5, 4.99999999), name1 = "x1", name2 = "x2"))
    expect_true(chk_gt_vector(x1 = 3:5, x2 = 2.9, name1 = "x1", name2 = "x2"))
    expect_true(chk_gt_vector(x1 = as.Date(c("2001-10-01", "2000-03-03")),
                                 x2 = as.Date(c("2001-01-01", "2000-03-02")),
                                 name1 = "x1", name2 = "x2"))
    expect_true(chk_gt_vector(x1 = as.Date(c("2001-10-01", NA)),
                                 x2 = as.Date(c("2001-01-01", "2000-03-03")),
                                 name1 = "x1", name2 = "x2"))
})

test_that("'chk_gt_vector' returns expected message with invalid inputs", {
    expect_identical(chk_gt_vector(x1 = c(2, 3), x2 = c(1, 4),
                                      name1 = "x1", name2 = "x2"),
                     "element 2 of 'x1' [3] is less than or equal to element 2 of 'x2' [4]")
    expect_identical(chk_gt_vector(x1 = c(5, 3), x2 = 4,
                                   name1 = "x1", name2 = "x2"),
                     "element 2 of 'x1' [3] is less than or equal to 'x2' [4]")
    expect_identical(chk_gt_vector(x1 = as.Date(c("2000-10-01", "2020-10-10")),
                                   x2 = as.Date(c("2000-10-01", "2019-10-10")),
                                      name1 = "x1", name2 = "x2"),
                     "element 1 of 'x1' [2000-10-01] is less than or equal to element 1 of 'x2' [2000-10-01]")
    expect_identical(chk_gt_vector(x1 = c(2, 3, 10), x2 = c(1, 4),
                                      name1 = "x1", name2 = "x2"),
                     "'x1' has length 3 and 'x2' has length 2")
})


## chk_is_logical_flag --------------------------------------------------------

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


## chk_le_scalar --------------------------------------------------------------

test_that("'chk_le_scalar' returns TRUE with valid inputs", {
    expect_true(chk_le_scalar(x1 = 3, x2 = 3.9, name1 = "x1", name2 = "x2"))
    expect_true(chk_le_scalar(x1 = as.Date("2001-10-01"), x2 = as.Date("2001-10-01"),
                                 name1 = "x1", name2 = "x2"))
    expect_true(chk_le_scalar(x1 = NA, x2 = 2.9, name1 = "x1", name2 = "x2"))
})

test_that("'chk_le_scalar' returns expected message with invalid inputs", {
    expect_identical(chk_le_scalar(x1 = 3, x2 = 2.9, name1 = "x1", name2 = "x2"),
                     "'x1' [3] is greater than 'x2' [2.9]")
    expect_identical(chk_le_scalar(x1 = as.Date("2000-10-01"), x2 = as.Date("2000-01-01"),
                                      name1 = "x1", name2 = "x2"),
                     "'x1' [2000-10-01] is greater than 'x2' [2000-01-01]")
    expect_identical(chk_le_scalar(x1 = as.Date("2000-10-02"), x2 = as.Date("2000-10-01"),
                                      name1 = "x1", name2 = "x2"),
                     "'x1' [2000-10-02] is greater than 'x2' [2000-10-01]")
})


## chk_le_vector --------------------------------------------------------------

test_that("'chk_le_vector' returns TRUE with valid inputs", {
    expect_true(chk_le_vector(x1 = 3:5, x2 = c(3.1, 4.5, 5), name1 = "x1", name2 = "x2"))
    expect_true(chk_le_vector(x1 = 3:5, x2 = 5.1, name1 = "x1", name2 = "x2"))
    expect_true(chk_le_vector(x1 = as.Date(c("2001-01-02", "2000-03-03")),
                              x2 = as.Date(c("2001-01-02", "2000-03-04")),
                              name1 = "x1", name2 = "x2"))
    expect_true(chk_le_vector(x1 = as.Date(c("2001-10-01", NA)),
                              x2 = as.Date(c("2001-11-01", "2000-03-03")),
                              name1 = "x1", name2 = "x2"))
})

test_that("'chk_le_vector' returns expected message with invalid inputs", {
    expect_identical(chk_le_vector(x1 = c(2, 3), x2 = c(3, 2),
                                   name1 = "x1", name2 = "x2"),
                     "element 2 of 'x1' [3] is greater than element 2 of 'x2' [2]")
    expect_identical(chk_le_vector(x1 = c(2, 3), x2 = 2,
                                   name1 = "x1", name2 = "x2"),
                     "element 2 of 'x1' [3] is greater than 'x2' [2]")
    expect_identical(chk_le_vector(x1 = as.Date(c("2000-10-01", "2000-01-01")),
                                   x2 = as.Date(c("2000-09-01", "2000-01-01")),
                                   name1 = "x1", name2 = "x2"),
                     "element 1 of 'x1' [2000-10-01] is greater than element 1 of 'x2' [2000-09-01]")
    expect_identical(chk_le_vector(x1 = c(2, 3, 4), x2 = c(3, 2),
                                   name1 = "x1", name2 = "x2"),
                     "'x1' has length 3 and 'x2' has length 2")
})


## chk_lt_scalar --------------------------------------------------------------

test_that("'chk_lt_scalar' returns TRUE with valid inputs", {
    expect_true(chk_lt_scalar(x1 = 3, x2 = 3.9, name1 = "x1", name2 = "x2"))
    expect_true(chk_lt_scalar(x1 = as.Date("2001-10-01"), x2 = as.Date("2001-12-01"),
                              name1 = "x1", name2 = "x2"))
    expect_true(chk_lt_scalar(x1 = NA, x2 = 2.9, name1 = "x1", name2 = "x2"))
})

test_that("'chk_lt_scalar' returns expected message with invalid inputs", {
    expect_identical(chk_lt_scalar(x1 = 2.9, x2 = 2.9, name1 = "x1", name2 = "x2"),
                     "'x1' [2.9] is greater than or equal to 'x2' [2.9]")
    expect_identical(chk_lt_scalar(x1 = as.Date("2000-10-01"), x2 = as.Date("2000-01-01"),
                                      name1 = "x1", name2 = "x2"),
                     "'x1' [2000-10-01] is greater than or equal to 'x2' [2000-01-01]")
    expect_identical(chk_lt_scalar(x1 = as.Date("2000-10-01"), x2 = as.Date("2000-10-01"),
                                      name1 = "x1", name2 = "x2"),
                     "'x1' [2000-10-01] is greater than or equal to 'x2' [2000-10-01]")
})


## chk_lt_vector --------------------------------------------------------------

test_that("'chk_lt_vector' returns TRUE with valid inputs", {
    expect_true(chk_lt_vector(x1 = 3:5, x2 = c(3.1, 4.5, 5.000000001), name1 = "x1", name2 = "x2"))
    expect_true(chk_lt_vector(x1 = 3:5, x2 = 5.1, name1 = "x1", name2 = "x2"))
    expect_true(chk_lt_vector(x1 = as.Date(c("2001-01-01", "2000-03-03")),
                              x2 = as.Date(c("2001-01-02", "2000-03-04")),
                              name1 = "x1", name2 = "x2"))
    expect_true(chk_lt_vector(x1 = as.Date(c("2001-10-01", NA)),
                              x2 = as.Date(c("2001-11-01", "2000-03-03")),
                              name1 = "x1", name2 = "x2"))
})

test_that("'chk_lt_vector' returns expected message with invalid inputs", {
    expect_identical(chk_lt_vector(x1 = c(2, 3), x2 = c(3, 2),
                                   name1 = "x1", name2 = "x2"),
                     "element 2 of 'x1' [3] is greater than or equal to element 2 of 'x2' [2]")
    expect_identical(chk_lt_vector(x1 = c(2, 3), x2 = 3,
                                   name1 = "x1", name2 = "x2"),
                     "element 2 of 'x1' [3] is greater than or equal to 'x2' [3]")
    expect_identical(chk_lt_vector(x1 = as.Date(c("2000-10-01", "2000-10-01")),
                                   x2 = as.Date(c("2000-09-01", "2000-10-01")),
                                   name1 = "x1", name2 = "x2"),
                     "element 1 of 'x1' [2000-10-01] is greater than or equal to element 1 of 'x2' [2000-09-01]")
    expect_identical(chk_lt_vector(x1 = c(2, 3, 4), x2 = c(3, 2),
                                   name1 = "x1", name2 = "x2"),
                     "'x1' has length 3 and 'x2' has length 2")
})


## chk_multiple_of ---------------------------------------------------------

test_that("'chk_multiple_of' returns TRUE with valid inputs", {
    expect_true(chk_multiple_of(x1 = 30L,
                                   x2 = 5L,
                                   name1 = "x1",
                                   name2 = "x2",
                                   null_ok = FALSE))
    expect_true(chk_multiple_of(x1 = NULL,
                                   x2 = 5L,
                                   name1 = "x1",
                                   name2 = "x2",
                                   null_ok = TRUE))
})

test_that("'chk_multiple_of' returns expected message with invalid argument", {
    expect_identical(chk_multiple_of(x1 = NULL,
                                        x2 = 5L,
                                        name1 = "x1",
                                        name2 = "x2",
                                        null_ok = FALSE),
                     "'x1' is NULL")
    expect_identical(chk_multiple_of(x1 = 31L,
                                        x2 = 5L,
                                        name1 = "x1",
                                        name2 = "x2",
                                        null_ok = FALSE),
                     "'x1' [31] is not a multiple of 'x2' [5]")
})


## chk_multiple_of_n -------------------------------------------------------

test_that("'chk_multiple_of_n' returns TRUE with valid inputs", {
    expect_true(chk_multiple_of_n(x = 30L,
                                     name = "x",
                                     n = 5L,
                                     null_ok = FALSE))
    expect_true(chk_multiple_of_n(x = NULL,
                                     name = "x",
                                     n = 5L,
                                     null_ok = TRUE))
})

test_that("'chk_multiple_of_n' returns expected message with invalid argument", {
    expect_identical(chk_multiple_of_n(x = NULL,
                                          name = "x",
                                          n = 5L,
                                          null_ok = FALSE),
                     "'x' is NULL")
    expect_identical(chk_multiple_of_n(x = 31L,
                                          name = "x",
                                          n = 5L,
                                          null_ok = FALSE),
                     "'x' [31] is not a multiple of 5")
})


## chk_non_negative_scalar

test_that("'chk_non_negative_scalar' returns TRUE with valid input", {
    expect_true(chk_non_negative_scalar(x = 1L,
                                        name = "x"))
    expect_true(chk_non_negative_scalar(x = 0,
                                        name = "x"))
    expect_true(chk_non_negative_scalar(x = NA_real_,
                                        name = "x"))
})

test_that("'chk_non_negative_scalar' returns expected message with invalid argument", {
    expect_identical(chk_non_negative_scalar(x = c(1, 1),
                                             name = "x"),
                     "'x' does not have length 1")
    expect_identical(chk_non_negative_scalar(x = "1",
                                             name = "x"),
                     "'x' does not have type \"numeric\"")
    expect_identical(chk_non_negative_scalar(x = -0.001,
                                             name = "x"),
                     "'x' [-0.001] is negative")
})


## chk_non_negative_vector -------------------------------------------------

test_that("'chk_non_negative_vector' returns TRUE with valid input", {
    expect_true(chk_non_negative_vector(x = c(0, 0.001),
                                        name = "x"))
    expect_true(chk_non_negative_vector(x = c(0.001, 0, Inf, NA),
                                        name = "x"))
})

test_that("'chk_non_negative_vector' returns expected message with invalid argument", {
    expect_identical(chk_non_negative_vector(x = c("1", "2"),
                                             name = "x"),
                     "'x' does not have type \"numeric\"")
    expect_identical(chk_non_negative_vector(x = c(0.1, -0.1),
                                             name = "x"),
                     "element 2 of 'x' [-0.1] is negative")
})


## chk_not_equal_integer_scalar -----------------------------------------------

test_that("'chk_not_equal_integer_scalar' returns TRUE with valid inputs", {
    expect_true(chk_not_equal_integer_scalar(x1 = 3L, x2 = 4L, name1 = "x1", name2 = "x2"))
    expect_true(chk_not_equal_integer_scalar(x1 = NA_integer_, x2 = 2L, name1 = "x1", name2 = "x2"))
})

test_that("'chk_not_equal_integer_scalar' returns expected message with invalid inputs", {
    expect_identical(chk_not_equal_integer_scalar(x1 = 2L, x2 = 2L, name1 = "x1", name2 = "x2"),
                     "'x1' [2] is equal to 'x2' [2]")
})


## chk_null_if_null -----------------------------------------------------------

test_that("'chk_null_if_null' returns TRUE with valid inputs", {
    expect_true(chk_null_if_null(x1 = NULL, x2 = NULL, name1 = "x1", name2 = "x2"))
    expect_true(chk_null_if_null(x1 = NULL, x2 = 1, name1 = "x1", name2 = "x2"))
    expect_true(chk_null_if_null(x1 = 1, x2 = 1, name1 = "x1", name2 = "x2"))

})

test_that("'chk_null_if_null' returns expected message with invalid inputs", {
    expect_identical(chk_null_if_null(x1 = 1, x2 = NULL, name1 = "x1", name2 = "x2"),
                     "'x1' is non-NULL [1] but 'x2' is NULL")
})


## chk_null_onlyif_null -----------------------------------------------------------

test_that("'chk_null_onlyif_null' returns TRUE with valid inputs", {
    expect_true(chk_null_onlyif_null(x1 = NULL, x2 = NULL, name1 = "x1", name2 = "x2"))
    expect_true(chk_null_onlyif_null(x1 = 1, x2 = NULL, name1 = "x1", name2 = "x2"))
    expect_true(chk_null_onlyif_null(x1 = 1, x2 = 1, name1 = "x1", name2 = "x2"))

})

test_that("'chk_null_onlyif_null' returns expected message with invalid inputs", {
    expect_identical(chk_null_onlyif_null(x1 = NULL, x2 = 1, name1 = "x1", name2 = "x2"),
                     "'x1' is NULL but 'x2' is non-NULL [1]")
})


## chk_null_ifonlyif_null -----------------------------------------------------------

test_that("'chk_null_ifonlyif_null' returns TRUE with valid inputs", {
    expect_true(chk_null_ifonlyif_null(x1 = NULL, x2 = NULL, name1 = "x1", name2 = "x2"))
    expect_true(chk_null_ifonlyif_null(x1 = 1, x2 = 1, name1 = "x1", name2 = "x2"))

})

test_that("'chk_null_ifonlyif_null' returns expected message with invalid inputs", {
    expect_identical(chk_null_ifonlyif_null(x1 = 1, x2 = NULL, name1 = "x1", name2 = "x2"),
                     "'x1' is non-NULL [1] but 'x2' is NULL")
    expect_identical(chk_null_ifonlyif_null(x1 = NULL, x2 = 1, name1 = "x1", name2 = "x2"),
                     "'x1' is NULL but 'x2' is non-NULL [1]")
})


## chk_pos_initial ------------------------------------------------------------

test_that("'chk_pos_initial' returns TRUE with valid input", {
    expect_true(chk_pos_initial(x = c(1L, 1L), name = "x", zero_ok = TRUE))
    expect_true(chk_pos_initial(x = c(1L, 1L), name = "x", zero_ok = FALSE))
    expect_true(chk_pos_initial(x = 1L, name = "x", zero_ok = TRUE))
    expect_true(chk_pos_initial(x = c(0L, 1L), name = "x", zero_ok = TRUE))
})
                                
test_that("'chk_pos_initial' returns expected message with invalid inputs", {
    expect_identical(chk_pos_initial(x = c(1L, 1L, 2L), name = "x", zero_ok = FALSE),
                     "element 3 of 'x' [2] is not equal to 1")
    expect_identical(chk_pos_initial(x = c(1L, 1L, 2L), name = "x", zero_ok = TRUE),
                     "element 3 of 'x' [2] is not equal to 0 or 1")
    expect_identical(chk_pos_initial(x = c(1L, 1L, 0L), name = "x", zero_ok = FALSE),
                     "element 3 of 'x' [0] is non-positive")
})


## chk_positive_scalar -----------------------------------------------------

test_that("'chk_positive_scalar' returns TRUE with valid input", {
    expect_true(chk_positive_scalar(x = 1L,
                                    name = "x"))
    expect_true(chk_positive_scalar(x = 0.001,
                                    name = "x"))
    expect_true(chk_positive_scalar(x = NA_real_,
                                    name = "x"))
})

test_that("'chk_positive_scalar' returns expected message with invalid argument", {
    expect_identical(chk_positive_scalar(x = c(1, 1),
                                   name = "x"),
                     "'x' does not have length 1")
    expect_identical(chk_positive_scalar(x = "1",
                                            name = "x"),
                     "'x' does not have type \"numeric\"")
    expect_identical(chk_positive_scalar(x = 0,
                                            name = "x"),
                     "'x' [0] is non-positive")
})


## chk_positive_vector -----------------------------------------------------

test_that("'chk_positive_vector' returns TRUE with valid input", {
    expect_true(chk_positive_vector(x = c(1, 0.001),
                                    name = "x"))
    expect_true(chk_positive_vector(x = c(0.001, Inf, NA),
                                    name = "x"))
})

test_that("'chk_positive_vector' returns expected message with invalid argument", {
    expect_identical(chk_positive_vector(x = c("1", "2"),
                                            name = "x"),
                     "'x' does not have type \"numeric\"")
    expect_identical(chk_positive_vector(x = c(0.1, 0),
                                            name = "x"),
                     "element 2 of 'x' [0] is non-positive")
})


## chk_is_string --------------------------------------------------------------

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


## chk_increasing -------------------------------------------------

test_that("'chk_increasing' returns TRUE with valid vector", {
    expect_true(chk_increasing(x = 1,
                               name = "x"))
    expect_true(chk_increasing(x = c(1:3, 3),
                               name = "x"))
    expect_true(chk_increasing(x = c(-Inf, 0, Inf),
                               name = "x"))
    expect_true(chk_increasing(x = as.Date(c("2001-01-01", "2002-01-01", "2002-01-01")),
                               name = "x"))
})

test_that("'chk_increasing' returns expected message with invalid argument", {
    expect_identical(chk_increasing(x = "a",
                                    name = "x"),
                     "'x' does not have class \"Date\" or \"numeric\"")
    expect_identical(chk_increasing(x = NA_integer_,
                                    name = "x"),
                     "'x' has NAs")
    expect_identical(chk_increasing(x = c(1, 3, 2),
                                    name = "x"),
                     "'x' is not increasing : element 2 [3] is greater than element 3 [2]")
    expect_identical(chk_increasing(x = as.Date(c("2002-01-02", "2002-01-01")),
                                    name = "x"),
                     paste("'x' is not increasing : element 1 [2002-01-02]",
                           "is greater than element 2 [2002-01-01]"))
})


## chk_strictly_increasing -------------------------------------------------

test_that("'chk_strictly_increasing' returns TRUE with valid vector", {
    expect_true(chk_strictly_increasing(x = 1,
                                           name = "x"))
    expect_true(chk_strictly_increasing(x = 1:3,
                                           name = "x"))
    expect_true(chk_strictly_increasing(x = c(-Inf, 0, Inf),
                                           name = "x"))
    expect_true(chk_strictly_increasing(x = as.Date(c("2001-01-01", "2002-01-01")),
                                           name = "x"))
})

test_that("'chk_strictly_increasing' returns expected message with invalid argument", {
    expect_identical(chk_strictly_increasing(x = "a",
                                                name = "x"),
                     "'x' does not have class \"Date\" or \"numeric\"")
    expect_identical(chk_strictly_increasing(x = NA_integer_,
                                                name = "x"),
                     "'x' has NAs")
    expect_identical(chk_strictly_increasing(x = c(1, 3, 2),
                                                name = "x"),
                     "'x' is not strictly increasing : element 2 [3] is greater than or equal to element 3 [2]")
    expect_identical(chk_strictly_increasing(x = as.Date(c("2002-01-01", "2002-01-01")),
                                                name = "x"),
                                                paste("'x' is not strictly increasing : element 1 [2002-01-01]",
                                                      "is greater than or equal to element 2 [2002-01-01]"))
})


## chk_valid_quantile ------------------------------------------------------

test_that("'chk_valid_quantile' returns TRUE with valid vector", {
    expect_true(chk_valid_quantile(x = c("1%", "99.9%", "0%", "100%", NA)))
    expect_true(chk_valid_quantile(x = character()))
})

test_that("'chk_valid_quantile' returns expected message with invalid argument", {
    expect_identical(chk_valid_quantile(x = "wrong"),
                     "\"wrong\" is not a valid quantile")
    expect_identical(chk_valid_quantile(x = "40.0.0%"),
                     "\"40.0.0%\" is not a valid quantile")
    expect_identical(chk_valid_quantile(x = "-10%"),
                     "\"-10%\" is not a valid quantile")
    expect_identical(chk_valid_quantile(x = "101%"),
                     "\"101%\" is not a valid quantile : greater than 100%")
})


## chk_indices_distinct -------------------------------------------------------

test_that("'chk_indices_distinct' returns TRUE with valid inputs", {
    expect_true(chk_indices_distinct(indices = list(1:2, 3L),
                                     names = c("i1", "i2"),
                                     exclude_zero = FALSE))
    expect_true(chk_indices_distinct(indices = list(0:2, 3L, 0L),
                                     names = c("i1", "i2", "i3"),
                                     exclude_zero = TRUE))
})

test_that("'chk_indices_distinct' returns expected message with invalid inputs", {
    expect_identical(chk_indices_distinct(indices = list(0:2, 0L),
                                          names = c("i1", "i2"),
                                          exclude_zero = FALSE),
                     "indices 'i1' [0,1,2], 'i2' [0] overlap")
})


## chk_length_equals ------------------------------------------------------------

test_that("'chk_length_equals' returns TRUE with valid inputs", {
    expect_true(chk_length_equals(x1 = 1:3,
                                  x2 = 3L,
                                  name1 = "x1",
                                  name2 = "x2"))
    expect_true(chk_length_equals(x1 = character(),
                                  x2 = 0L,
                                  name1 = "x1",
                                  name2 = "x2"))
})

test_that("'chk_length_equals' returns expected message with invalid inputs", {
    expect_identical(chk_length_equals(x1 = integer(),
                                       x2 = 3L,
                                       name1 = "x1",
                                       name2 = "x2"),
                     "length of 'x1' [0] not equal to 'x2' [3]")
    expect_identical(chk_length_equals(x1 = 1L,
                                       x2 = 0L,
                                       name1 = "x1",
                                       name2 = "x2"),
                     "length of 'x1' [1] not equal to 'x2' [0]")
})


## chk_length_same ------------------------------------------------------------

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


## chk_length_same_or_1 -------------------------------------------------------

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


## chk_lengths_elements_equal_vec -------------------------------------------------------

test_that("'chk_lengths_elements_equal_vec' returns TRUE with valid inputs", {
    expect_true(chk_lengths_elements_equal_vec(x1 = list(1:3, 1:2),
                                               x2 = 3:2,
                                               name1 = "x1",
                                               name2 = "x2"))
    expect_true(chk_lengths_elements_equal_vec(x1 = list(),
                                               x2 = integer(),
                                               name1 = "x1",
                                               name2 = "x2"))
})

test_that("'chk_lengths_elements_equal_vec' returns expected message with invalid inputs", {
    expect_identical(chk_lengths_elements_equal_vec(x1 = list(1:3, 1:2),
                                                    x2 = 3:4,
                                                    name1 = "x1",
                                                    name2 = "x2"),
                     "length of element 2 of 'x1' [2] not equal to element 2 of 'x2' [4]")
})


## chk_lt_break_max_age -------------------------------------------------------

test_that("'chk_lt_break_max_age' returns TRUE with valid inputs", {
    expect_true(chk_lt_break_max_age(age = 5L,
                                     break_max = 10L,
                                     date = as.Date("2005-01-02"),
                                     dob = as.Date("2000-01-01"),
                                     unit = "year"))
})

test_that("'chk_lt_break_max_age' returns expected message with invalid dates", {
    expect_identical(chk_lt_break_max_age(age = 5L,
                                          break_max = 5L,
                                          date = as.Date("2005-01-02"),
                                          dob = as.Date("2000-01-01"),
                                          unit = "year"),
                     "'date' [\"2005-01-02\"] and 'dob' [\"2000-01-01\"] imply an age of 5 years, which is greater than or equal to 'break_max' [5 years]")
})


## chk_lt_break_max_date ------------------------------------------------------

test_that("'chk_lt_break_max_date' returns TRUE with valid inputs", {
    expect_true(chk_lt_break_max_date(date = as.Date(c("2001-02-20", "2005-11-24", NA)),
                                      break_max = as.Date("2010-01-01")))
    expect_true(chk_lt_break_max_date(date = as.Date(c("2001-02-20", "2005-11-24", NA)),
                                      break_max = NULL))
})

test_that("'chk_lt_break_max_date' returns expected message with invalid dates", {
    expect_identical(chk_lt_break_max_date(date = as.Date(c("2001-02-20", "2005-11-24", NA)),
                                           break_max = "2002-01-01"),
                     "'date' has value [\"2005-11-24\"] that is greater than or equal to 'break_max' [\"2002-01-01\"]")
})

    
## chk_names_complete ---------------------------------------------------------

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
    x_wrong <- unname(x)
    expect_identical(chk_names_complete(x = x_wrong,
                                        name = "x"),
                     "'x' does not have names")
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


## chk_names_dimnames_complete ------------------------------------------------

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


## chk_quantile_increasing ---------------------------------------------------

test_that("'chk_quantile_increasing' returns TRUE with valid input", {
    expect_true(chk_quantile_increasing(x = c("2.5%", "50%", "97.5%"),
                                         name = "x"))
    expect_true(chk_quantile_increasing(x = "2.5%",
                                         name = "x"))
    expect_true(chk_quantile_increasing(x = character(),
                                         name = "x"))
})

test_that("'chk_quantile_increasing' returns expected message with invalid argument", {
    expect_identical(chk_quantile_increasing(x = c("2.5%", "2.5%", "97.5%"),
                                              name = "x"),
                     "'x' is not strictly increasing : element 1 [2.5%] is greater than or equal to element 2 [2.5%]")
})

## chk_trans_list -------------------------------------------------------------

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


## chk_zero_if_zero -----------------------------------------------------------

test_that("'chk_zero_if_zero' returns TRUE with valid inputs", {
    expect_true(chk_zero_if_zero(x1 = 0, x2 = 0, name1 = "x1", name2 = "x2"))
    expect_true(chk_zero_if_zero(x1 = 0, x2 = 1, name1 = "x1", name2 = "x2"))
    expect_true(chk_zero_if_zero(x1 = 1, x2 = 1, name1 = "x1", name2 = "x2"))

})

test_that("'chk_zero_if_zero' returns expected message with invalid inputs", {
    expect_identical(chk_zero_if_zero(x1 = 1, x2 = 0, name1 = "x1", name2 = "x2"),
                     "'x1' [1] does not equal 0 but 'x2' equals 0")
})


## chk_zero_onlyif_zero -----------------------------------------------------------

test_that("'chk_zero_onlyif_zero' returns TRUE with valid inputs", {
    expect_true(chk_zero_onlyif_zero(x1 = 0, x2 = 0, name1 = "x1", name2 = "x2"))
    expect_true(chk_zero_onlyif_zero(x1 = 1, x2 = 0, name1 = "x1", name2 = "x2"))
    expect_true(chk_zero_onlyif_zero(x1 = 1, x2 = 1, name1 = "x1", name2 = "x2"))

})

test_that("'chk_zero_onlyif_zero' returns expected message with invalid inputs", {
    expect_identical(chk_zero_onlyif_zero(x1 = 0, x2 = 1, name1 = "x1", name2 = "x2"),
                     "'x1' equals 0 but 'x2' [1] does not equal 0")
})


## chk_zero_ifonlyif_zero -----------------------------------------------------------

test_that("'chk_zero_ifonlyif_zero' returns TRUE with valid inputs", {
    expect_true(chk_zero_ifonlyif_zero(x1 = 0, x2 = 0, name1 = "x1", name2 = "x2"))
    expect_true(chk_zero_ifonlyif_zero(x1 = 1, x2 = 1, name1 = "x1", name2 = "x2"))

})

test_that("'chk_zero_ifonlyif_zero' returns expected message with invalid inputs", {
    expect_identical(chk_zero_ifonlyif_zero(x1 = 1, x2 = 0, name1 = "x1", name2 = "x2"),
                     "'x1' [1] does not equal 0 but 'x2' equals 0")
    expect_identical(chk_zero_ifonlyif_zero(x1 = 0, x2 = 1, name1 = "x1", name2 = "x2"),
                     "'x1' equals 0 but 'x2' [1] does not equal 0")
})

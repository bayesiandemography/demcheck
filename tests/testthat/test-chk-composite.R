
context("composite")

## chk_is_first_day_time_unit

test_that("'chk_is_first_day_time_unit' returns TRUE with valid dates", {
    expect_true(chk_is_first_day_time_unit(x = as.Date(c("2001-01-01", "2002-01-01")),
                                           name = "x",
                                           time_unit = "month"))
    expect_true(chk_is_first_day_time_unit(x = as.Date(c("2001-10-01", "2002-01-01")),
                                           name = "x",
                                           time_unit = "quarter"))
})

test_that("'chk_is_first_day_time_unit' returns expected message with invalid dates", {
    expect_identical(chk_is_first_day_time_unit(x = as.Date(c("2001-01-01", "2002-01-02")),
                                                name = "x",
                                                time_unit = "month"),
                     "element 2 [\"2002-01-02\"] of 'x' is not the first day of the month")
    expect_identical(chk_is_first_day_time_unit(x = as.Date(c("2001-01-01", "2001-02-01")),
                                                name = "x",
                                                time_unit = "quarter"),
                     "element 2 [\"2001-02-01\"] of 'x' is not the first day of the quarter")
})


## chk_is_first_day_time_unit_consec

test_that("'chk_is_first_day_time_unit_consec' returns TRUE with valid dates", {
    expect_true(chk_is_first_day_time_unit_consec(x = as.Date(c("2001-01-01", "2001-02-01")),
                                              name = "x",
                                              time_unit = "month"))
    expect_true(chk_is_first_day_time_unit_consec(x = as.Date(c("2001-01-01", "2001-04-01")),
                                              name = "x",
                                              time_unit = "quarter"))
    expect_true(chk_is_first_day_time_unit_consec(x = as.Date("2001-01-01"),
                                              name = "x",
                                              time_unit = "quarter"))
    expect_true(chk_is_first_day_time_unit_consec(x = as.Date(c("2000-10-1", "2001-01-01")),
                                              name = "x",
                                              time_unit = "quarter"))
})

test_that("'chk_is_first_day_time_unit_consec' returns expected message with invalid dates", {
    expect_identical(chk_is_first_day_time_unit_consec(x = as.Date(c("2001-01-01", "2001-03-01")),
                                                       name = "x",
                                                       time_unit = "month"),
                     "dates \"2001-01-01\" and \"2001-03-01\" in 'x' do not belong to consecutive months")
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

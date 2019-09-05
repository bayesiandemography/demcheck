
context("err-tdy")

## err_tdy_breaks

test_that("'err_tdy_breaks' returns breaks with valid input", {
    expect_identical(err_tdy_breaks(x = 0:4,
                                    name = "x"),
                     0:4)
    expect_identical(err_tdy_breaks(x = c(0, 5),
                                    name = "x"),
                     c(0L, 5L))
    expect_identical(err_tdy_breaks(x = c(-5, 0, 1),
                                    name = "x"),
                     c(-5L, 0L, 1L))
    expect_identical(err_tdy_breaks(x = c(-5, 0, 1),
                                    name = "x"),
                     c(-5L, 0L, 1L))
    expect_identical(err_tdy_breaks(x = 100,
                                    name = "x"),
                     100L)
})
    
test_that("'err_tdy_breaks' raises expected error with invalid input", {
    expect_error(err_tdy_breaks(x = integer(),
                                name = "x"),
                 "'x' has length 0")
    expect_error(err_tdy_breaks(x = c(0L, NA),
                                name = "x"),
                 "'x' has NAs")
    expect_error(err_tdy_breaks(x = c(0L, Inf),
                                name = "x"),
                 "'x' has infinite values")
    expect_error(err_tdy_breaks(x = c(0L, 1.1),
                                name = "x"),
                 "value '1.1' in 'x' not equivalent to integer")
    expect_error(err_tdy_breaks(x = c(1L, 0L),
                                name = "x"),
                 "'x' is not strictly increasing : element 1 \\[1\\] is greater than or equal to element 2 \\[0\\]")
})    


## err_tdy_date

test_that("'err_tdy_date' returns dates with valid input", {
    x <- c("2001-01-01", "2002-01-01")
    ans_obtained <- err_tdy_date(x = x, name = "x")
    ans_expected <- as.Date(x)
    expect_identical(ans_obtained, ans_expected)
    x <- as.Date(c("2001-01-01", "2002-01-01"))
    ans_obtained <- err_tdy_date(x = x, name = "x")
    ans_expected <- as.Date(x)
    expect_identical(ans_obtained, ans_expected)
    x <- c("2001/1/1", "2002/1/1")
    ans_obtained <- err_tdy_date(x = x, name = "x")
    ans_expected <- as.Date(x)
    expect_identical(ans_obtained, ans_expected)
    x <- character()
    ans_obtained <- err_tdy_date(x = x, name = "x")
    ans_expected <- as.Date(x)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'err_tdy_date' raises expected error with invalid input", {
    expect_error(err_tdy_date(x = 1,
                              name = "x"),
                 "'x' \\[1\\] not equivalent to dates : 'origin' must be supplied")
    expect_error(err_tdy_date(x = letters,
                              name = "x"),
                 "'x' \\[a, b, c, \\.\\.\\.\\] not equivalent to dates : character string is not in a standard unambiguous format")
    expect_error(err_tdy_date (x = c("2000-01-01", "a-02-01"),
                               name = "x"),
                 "value 'a-02-01' in 'x' not equivalent to date")
})


## err_tdy_date_dob

test_that("'err_tdy_date_dob' returns dates with valid input", {
    date <- as.Date(c("2001-01-01", "2002-01-01"))
    dob <- as.Date("2000-06-30")
    expect_identical(err_tdy_date_dob(date = date, dob = dob),
                     list(date = as.POSIXlt(date),
                          dob = rep(as.POSIXlt(dob), 2)))
})


## err_tdy_first_month

test_that("'err_tdy_first_month' returns day and month with valid input", {
    expect_identical(err_tdy_first_month("January"),
                     "Jan")
    expect_identical(err_tdy_first_month("Jan"),
                     "Jan")
    expect_identical(err_tdy_first_month("feb"),
                     "Feb")
    expect_identical(err_tdy_first_month("march"),
                     "Mar")
})

test_that("'err_tdy_first_month' raises expected error with invalid input", {
    expect_error(err_tdy_first_month(x = "wrong",
                                     name = "x"),
                 "invalid value for 'x' : \"wrong\"")
})
    

## err_tdy_integer_scalar

test_that("'err_tdy_integer_scalar' works with valid inputs", {
    expect_identical(err_tdy_integer_scalar(x = 1, name = "x"),
                     1L)
    expect_identical(err_tdy_integer_scalar(x = NA, name = "x"),
                     NA_integer_)
    expect_identical(err_tdy_integer_scalar(x = -Inf, name = "x", inf_ok = TRUE),
                     -Inf)
    expect_identical(err_tdy_integer_scalar(x = Inf, name = "x", inf_ok = TRUE),
                     Inf)
})

test_that("'err_tdy_integer_scalar' raises expected error with invalid input", {
    expect_error(err_tdy_integer_scalar(x = 0.2,
                                 name = "x"),
                 "'x' \\[0.2\\] not equivalent to integer")
    expect_error(err_tdy_integer_scalar(x = Inf,
                                 name = "x"),
                 "'x' \\[Inf\\] not equivalent to integer")
    expect_error(err_tdy_integer_scalar(x = "a",
                                 name = "x"),
                 "'x' \\[a\\] not equivalent to integer")
    expect_error(err_tdy_integer_scalar(x = character(), name = "x"),
                 "'x' does not have length 1")
})


## err_tdy_integer_vector

test_that("'err_tdy_integer_vector' works with valid inputs", {
    expect_identical(err_tdy_integer_vector(x = c(1, NA, 22, -1), name = "x"),
                     c(1L, NA, 22L, -1L))
    expect_identical(err_tdy_integer_vector(x = character(), name = "x"),
                     integer())
})

test_that("'err_tdy_integer_vector' raises expected error with invalid input", {
    expect_error(err_tdy_integer_vector(x = c(0.2, 3),
                                 name = "x"),
                 "value '0.2' in 'x' not equivalent to integer")
    expect_error(err_tdy_integer_vector(x = c(0, Inf),
                                 name = "x"),
                 "value 'Inf' in 'x' not equivalent to integer")
    expect_error(err_tdy_integer_vector(x = c(0, Inf, 0.1),
                                 name = "x"),
                 "value 'Inf' in 'x' not equivalent to integer")
    expect_error(err_tdy_integer_vector(x = "a",
                                 name = "x"),
                 "value 'a' in 'x' not equivalent to integer")
})


## err_tdy_many_to_one

test_that("'err_tdy_many_to_one' works with valid inputs", {
    x <- data.frame(a = 1:2, b = c("z", "z"))
    expect_identical(err_tdy_many_to_one(x = x,
                                         name = "x"),
                     data.frame(a = as.character(1:2), b = c("z", "z"), stringsAsFactors = FALSE))
})

test_that("'err_tdy_many_to_one' raises expected error with invalid input", {
    x <- "wrong"
    expect_error(err_tdy_many_to_one(x = x,
                                     name = "x"),
                 "'x' is not a data.frame")
    x <- data.frame(a = 1:2, b = c("z", "z"), c = 3:4)
    expect_error(err_tdy_many_to_one(x = x,
                                     name = "x"),
                 "'x' does not have 2 columns")
    x <- data.frame(a = character(), b = character())
    expect_error(err_tdy_many_to_one(x = x,
                                     name = "x"),
                 "'x' has 0 rows")
    x <- data.frame(a = 1:2, b = c(NA, "z"))
    expect_error(err_tdy_many_to_one(x = x,
                                     name = "x"),
                 "column 2 of 'x' has NAs")
    x <- data.frame(a = 1:2, b = c("y", "z"))
    expect_error(err_tdy_many_to_one(x = x,
                                     name = "x"),
                 "neither column of 'x' has duplicates, as required for many-to-one mapping")
    x <- data.frame(a = c(1, 1), b = c("y", "y"))
    expect_error(err_tdy_many_to_one(x = x,
                                     name = "x"),
                 "neither column of 'x' has entirely unique values, as required for many-to-one mapping")
})


## err_tdy_positive_integer_scalar

test_that("'err_tdy_positive_integer_scalar' works with valid inputs", {
    expect_identical(err_tdy_positive_integer_scalar(x = 1, name = "x"),
                     1L)
    expect_identical(err_tdy_integer_scalar(x = Inf, name = "x", inf_ok = TRUE),
                     Inf)
})

test_that("'err_tdy_positive_integer_scalar' raises expected error with invalid input", {
    expect_error(err_tdy_positive_integer_scalar(x = 0.1,
                                                 name = "x"),
                 "'x' \\[0.1\\] not equivalent to integer")
    expect_error(err_tdy_positive_integer_scalar(x = -Inf, name = "x", inf_ok = TRUE),
                 "'x' \\[-Inf\\] is non-positive")
    expect_error(err_tdy_positive_integer_scalar(x = Inf, name = "x"),
                 "'x' \\[Inf\\] not equivalent to integer")
})


## err_tdy_same_length

test_that("'err_tdy_same_length' works with valid inputs", {
    expect_identical(err_tdy_same_length(x1 = 1L, x2 = 2, name1 = "x1", name2 = "x2"),
                     list(x1 = 1L, x2 = 2))
    expect_identical(err_tdy_same_length(x1 = 1L, x2 = 1:2, name1 = "x1", name2 = "x2"),
                     list(x1 = c(1L, 1L), x2 = 1:2))
    expect_identical(err_tdy_same_length(x1 = 1:2, x2 = 2, name1 = "x1", name2 = "x2"),
                     list(x1 = 1:2, x2 = c(2, 2)))
    expect_identical(err_tdy_same_length(x1 = 1:2, x2 = 2:1, name1 = "x1", name2 = "x2"),
                     list(x1 = 1:2, x2 = 2:1))
})


## err_tdy_unit

test_that("'err_tdy_unit' works with valid inputs", {
    expect_identical(err_tdy_unit(x = NULL, name = "x"),
                     "year")
    for (val in c("year", "years",
                  "quarter", "quarters",
                  "month", "months",
                  "1 year", "1 years",
                  "33 year", "33 years"))
    expect_identical(err_tdy_unit(x = val, name = "x"),
                     val)
})

test_that("'err_tdy_unit' raises expected error with invalid input", {
    expect_error(err_tdy_unit(x = NA_character_,
                              name = "x"),
                 "'x' is NA")
    expect_error(err_tdy_unit(x = "wrong",
                              name = "x"),
                 "'x' has invalid value \\[\"wrong\"\\]")
    expect_error(err_tdy_unit(x = "1 month",
                              name = "x"),
                 "'x' has invalid value \\[\"1 month\"\\]")
    expect_error(err_tdy_unit(x = "0 year",
                              name = "x"),
                 "'x' has invalid value \\[\"0 year\"\\] : number of years less than 1")
    expect_error(err_tdy_unit(x = "-5 years",
                              name = "x"),
                 "'x' has invalid value \\[\"-5 years\"\\] : number of years less than 1")
})

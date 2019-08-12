
context("simple")

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


## err_tdy_integer

test_that("'err_tdy_integer' works with valid inputs", {
    expect_identical(err_tdy_integer(x = c(1, NA, 22, -1), name = "x"),
                     c(1L, NA, 22L, -1L))
    expect_identical(err_tdy_integer(x = character(), name = "x"),
                     integer())
})

test_that("'err_tdy_integer' raises expected error with invalid input", {
    expect_error(err_tdy_integer(x = c(0.2, 3),
                                 name = "x"),
                 "value '0.2' in 'x' not equivalent to integer")
    expect_error(err_tdy_integer(x = c(0, Inf),
                                 name = "x"),
                 "value 'Inf' in 'x' not equivalent to integer")
    expect_error(err_tdy_integer(x = c(0, Inf, 0.1),
                                 name = "x"),
                 "value 'Inf' in 'x' not equivalent to integer")
    expect_error(err_tdy_integer(x = "a",
                                 name = "x"),
                 "value 'a' in 'x' not equivalent to integer")
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


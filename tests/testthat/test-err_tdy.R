
context("err-tdy")

## err_tdy_break_min_max_date

test_that("'err_tdy_break_min_max_date' works with valid input", {
    expect_identical(err_tdy_break_min_max_date(break_min = "2000-01-01",
                                                break_max = "2001-01-01",
                                                unit = "year",
                                                null_ok = TRUE,
                                                equal_ok = FALSE),
                     list(break_min = as.Date("2000-01-01"),
                          break_max = as.Date("2001-01-01")))
    expect_identical(err_tdy_break_min_max_date(break_min = "2000-01-01",
                                                break_max = "2000-01-01",
                                                unit = "year",
                                                null_ok = TRUE,
                                                equal_ok = TRUE),
                     list(break_min = as.Date("2000-01-01"),
                          break_max = as.Date("2000-01-01")))
    expect_identical(err_tdy_break_min_max_date(break_min = "2000-01-01",
                                                break_max = "2001-01-01",
                                                unit = "quarter",
                                                null_ok = TRUE,
                                                equal_ok = FALSE),
                     list(break_min = as.Date("2000-01-01"),
                          break_max = as.Date("2001-01-01")))
    expect_identical(err_tdy_break_min_max_date(break_min = "2000-01-01",
                                                break_max = "2001-01-01",
                                                unit = "month",
                                                null_ok = TRUE,
                                                equal_ok = FALSE),
                     list(break_min = as.Date("2000-01-01"),
                          break_max = as.Date("2001-01-01")))
    expect_identical(err_tdy_break_min_max_date(break_min = "2000-01-01",
                                                break_max = "2001-04-01",
                                                unit = "quarter",
                                                null_ok = TRUE,
                                                equal_ok = FALSE),
                     list(break_min = as.Date("2000-01-01"),
                          break_max = as.Date("2001-04-01")))
    expect_identical(err_tdy_break_min_max_date(break_min = NULL,
                                                break_max = "2001-04-01",
                                                unit = "quarter",
                                                null_ok = TRUE,
                                                equal_ok = FALSE),
                     list(break_min = NULL,
                          break_max = as.Date("2001-04-01")))
    expect_identical(err_tdy_break_min_max_date(break_min = "2000-01-01",
                                                break_max = NULL,
                                                unit = "quarter",
                                                null_ok = TRUE,
                                                equal_ok = FALSE),
                     list(break_min = as.Date("2000-01-01"),
                          break_max = NULL))
})

test_that("'err_tdy_break_min_max_date' raises expected error with invalid input", {
    expect_error(err_tdy_break_min_max_date(break_min = NULL,
                                            break_max = "2001-01-01",
                                            unit = "year",
                                            null_ok = FALSE,
                                            equal_ok = FALSE),
                 "'break_min' is NULL")
    expect_error(err_tdy_break_min_max_date(break_min = "2001-01-01",
                                            break_max = NULL,
                                            unit = "year",
                                            null_ok = FALSE,
                                            equal_ok = FALSE),
                 "'break_max' is NULL")
    expect_error(err_tdy_break_min_max_date(break_min = NULL,
                                            break_max = NULL,
                                            unit = "year",
                                            null_ok = TRUE,
                                            equal_ok = FALSE),
                 "'break_min' and 'break_max' both NULL")
    expect_error(err_tdy_break_min_max_date(break_min = "2000-01-01",
                                            break_max = "2000-01-01",
                                            unit = "year",
                                            null_ok = TRUE,
                                            equal_ok = FALSE),
                 "'break_max' \\[2000-01-01\\] is less than or equal to 'break_min' \\[2000-01-01\\]")
})


## err_tdy_break_min_max_integer

test_that("'err_tdy_break_min_max_integer' works with valid input", {
    expect_identical(err_tdy_break_min_max_integer(break_min = 0,
                                                   break_max = 100,
                                                   null_ok = TRUE,
                                                   equal_ok = FALSE),
                     list(break_min = 0L,
                          break_max = 100L))
    expect_identical(err_tdy_break_min_max_integer(break_min = 0,
                                                   break_max = 0,
                                                   null_ok = TRUE,
                                                   equal_ok = TRUE),
                     list(break_min = 0L,
                          break_max = 0L))
    expect_identical(err_tdy_break_min_max_integer(break_min = 0,
                                                   break_max = 400,
                                                   null_ok = TRUE,
                                                   equal_ok = FALSE),
                     list(break_min = 0L,
                          break_max = 400L))
    expect_identical(err_tdy_break_min_max_integer(break_min = 0L,
                                                   break_max = 1200L,
                                                   null_ok = TRUE,
                                                   equal_ok = FALSE),
                     list(break_min = 0L,
                          break_max = 1200L))
    expect_identical(err_tdy_break_min_max_integer(break_min = 100,
                                                   break_max = 200,
                                                   null_ok = TRUE,
                                                   equal_ok = FALSE),
                     list(break_min = 100L,
                          break_max = 200L))
    expect_identical(err_tdy_break_min_max_integer(break_min = NULL,
                                                   break_max = 400,
                                                   null_ok = TRUE,
                                                   equal_ok = FALSE),
                     list(break_min = NULL,
                          break_max = 400L))
    expect_identical(err_tdy_break_min_max_integer(break_min = 0L,
                                                   break_max = NULL,
                                                   null_ok = TRUE,
                                                   equal_ok = FALSE),
                     list(break_min = 0L,
                          break_max = NULL))
})

test_that("'err_tdy_break_min_max_integer' raises expected error with invalid input", {
    expect_error(err_tdy_break_min_max_integer(break_min = NULL,
                                               break_max = 100,
                                               null_ok = FALSE,
                                               equal_ok = FALSE),
                 "'break_min' is NULL")
    expect_error(err_tdy_break_min_max_integer(break_min = 0,
                                               break_max = NULL,
                                               null_ok = FALSE,
                                               equal_ok = FALSE),
                 "'break_max' is NULL")
    expect_error(err_tdy_break_min_max_integer(break_min = NULL,
                                               break_max = NULL,
                                               null_ok = TRUE,
                                               equal_ok = FALSE),
                 "'break_min' and 'break_max' both NULL")
    expect_error(err_tdy_break_min_max_integer(break_min = 10,
                                               break_max = 10,
                                               null_ok = TRUE,
                                               equal_ok = FALSE),
                 "'break_max' \\[10\\] is less than or equal to 'break_min' \\[10\\]")
})


## err_tdy_breaks_date_cohort

test_that("'err_tdy_breaks_date_cohort' returns breaks with valid input", {
    expect_identical(err_tdy_breaks_date_cohort(breaks = c("2000-01-01", "2001-01-01"),
                                         open_first = FALSE),
                     as.Date(c("2000-01-01", "2001-01-01")))
    expect_identical(err_tdy_breaks_date_cohort(breaks = c("2000-02-01", "2001-02-01"),
                                         open_first = FALSE),
                     as.Date(c("2000-02-01", "2001-02-01")))
    expect_identical(err_tdy_breaks_date_cohort(breaks = character(),
                                         open_first = FALSE),
                     as.Date(character()))
    expect_identical(err_tdy_breaks_date_cohort(breaks = as.Date("2000-01-01"),
                                         open_first = TRUE),
                     as.Date("2000-01-01"))
})
    
test_that("'err_tdy_breaks_date_cohort' raises expected error with invalid input", {
    expect_error(err_tdy_breaks_date_cohort(breaks = character(),
                                     open_first = TRUE),
                 "'breaks' has length 0 but 'open_first' is TRUE")
    expect_error(err_tdy_breaks_date_cohort(breaks = "2002-01-01",
                                     open_first = FALSE),
                 "'breaks' has length 1 but 'open_first' is FALSE")
    expect_error(err_tdy_breaks_date_cohort(breaks = c("2002-01-01", "2001-01-01"),
                                     open_first = FALSE),
                 paste("'breaks' is not strictly increasing : element 1 \\[2002-01-01\\] is",
                       "greater than or equal to element 2 \\[2001-01-01\\]"))
    expect_error(err_tdy_breaks_date_cohort(breaks = c("2001-01-01", NA),
                                     open_first = FALSE),
                 "'breaks' has NAs")
    expect_error(err_tdy_breaks_date_cohort(breaks = c("2002-01-01", "2001-01-01"),
                                     open_first = FALSE),
                 paste("'breaks' is not strictly increasing : element 1 \\[2002-01-01\\] is",
                       "greater than or equal to element 2 \\[2001-01-01\\]"))
})


## err_tdy_breaks_date_period

test_that("'err_tdy_breaks_date_period' returns breaks with valid input", {
    expect_identical(err_tdy_breaks_date_period(breaks = c("2000-01-01", "2001-01-01")),
                     as.Date(c("2000-01-01", "2001-01-01")))
    expect_identical(err_tdy_breaks_date_period(breaks = c("2000-02-01", "2001-02-01")),
                     as.Date(c("2000-02-01", "2001-02-01")))
    expect_identical(err_tdy_breaks_date_period(breaks = character()),
                     as.Date(character()))
})
    
test_that("'err_tdy_breaks_date_period' raises expected error with invalid input", {
    expect_error(err_tdy_breaks_date_period(breaks = "2002-01-01"),
                 "'breaks' has length 1")
    expect_error(err_tdy_breaks_date_period(breaks = c("2002-01-01", "2001-01-01")),
                 paste("'breaks' is not strictly increasing : element 1 \\[2002-01-01\\] is",
                       "greater than or equal to element 2 \\[2001-01-01\\]"))
    expect_error(err_tdy_breaks_date_period(breaks = c("2001-01-01", NA)),
                 "'breaks' has NAs")
    expect_error(err_tdy_breaks_date_period(breaks = c("2002-01-01", "2001-01-01")),
                 paste("'breaks' is not strictly increasing : element 1 \\[2002-01-01\\] is",
                       "greater than or equal to element 2 \\[2001-01-01\\]"))
})


## err_tdy_breaks_integer_age

test_that("'err_tdy_breaks_integer_age' returns breaks with valid input", {
    expect_identical(err_tdy_breaks_integer_age(breaks = 0:4,
                                                open_last = FALSE),
                     0:4)
    expect_identical(err_tdy_breaks_integer_age(breaks = c(0, 5),
                                                open_last = FALSE),
                     c(0L, 5L))
    expect_identical(err_tdy_breaks_integer_age(breaks = c(0, 1),
                                                open_last = FALSE),
                     c(0L, 1L))
    expect_identical(err_tdy_breaks_integer_age(breaks = c(100, 101),
                                                open_last = FALSE),
                     c(100L, 101L))
    expect_identical(err_tdy_breaks_integer_age(breaks = integer(),
                                                open_last = FALSE),
                     integer())
    expect_identical(err_tdy_breaks_integer_age(breaks = 0,
                                                open_last = TRUE),
                     0L)
})
    
test_that("'err_tdy_breaks_integer_age' raises expected error with invalid input", {
    expect_error(err_tdy_breaks_integer_age(breaks = numeric(),
                                            open_last = TRUE),
                 "'breaks' has length 0 but 'open_last' is TRUE")
    expect_error(err_tdy_breaks_integer_age(breaks = 10,
                                            open_last = FALSE),
                 "'breaks' has length 1 but 'open_last' is FALSE")
    expect_error(err_tdy_breaks_integer_age(breaks = c(-5, 0, 1),
                                            open_last = TRUE),
                 "element 1 of 'breaks' \\[-5\\] is negative")
    expect_error(err_tdy_breaks_integer_age(breaks = c(0L, NA),
                                            open_last = FALSE),
                 "'breaks' has NAs")
    expect_error(err_tdy_breaks_integer_age(breaks = c(0L, Inf),
                                            open_last = FALSE),
                 "'breaks' has infinite values")
    expect_error(err_tdy_breaks_integer_age(breaks = c(0L, 1.1),
                                            open_last = FALSE),
                 "value '1.1' in 'breaks' not equivalent to integer")
    expect_error(err_tdy_breaks_integer_age(breaks = c(1L, 0L),
                                            open_last = FALSE),
                 "'breaks' is not strictly increasing : element 1 \\[1\\] is greater than or equal to element 2 \\[0\\]")
})

## err_tdy_breaks_integer_enum

test_that("'err_tdy_breaks_integer_enum' returns breaks with valid input", {
    expect_identical(err_tdy_breaks_integer_enum(breaks = 0:4,
                                                 open_first = FALSE,
                                                 open_last = FALSE),
                     0:4)
    expect_identical(err_tdy_breaks_integer_enum(breaks = c(-100, 0, 5),
                                                 open_first = FALSE,
                                                 open_last = FALSE),
                     c(-100L, 0L, 5L))
    expect_identical(err_tdy_breaks_integer_enum(breaks = c(0, 1),
                                                 open_first = TRUE,
                                                 open_last = FALSE),
                     c(0L, 1L))
    expect_identical(err_tdy_breaks_integer_enum(breaks = c(100, 101),
                                                 open_first = TRUE,
                                                 open_last = FALSE),
                     c(100L, 101L))
    expect_identical(err_tdy_breaks_integer_enum(breaks = integer(),
                                                 open_first = FALSE,
                                                 open_last = FALSE),
                     integer())
    expect_identical(err_tdy_breaks_integer_enum(breaks = 0,
                                                 open_first = TRUE,
                                                 open_last = TRUE),
                     0L)
})
    
test_that("'err_tdy_breaks_integer_enum' raises expected error with invalid input", {
    expect_error(err_tdy_breaks_integer_enum(breaks = numeric(),
                                             open_first = FALSE,
                                             open_last = TRUE),
                 "'breaks' has length 0 but 'open_last' is TRUE")
    expect_error(err_tdy_breaks_integer_enum(breaks = numeric(),
                                             open_first = TRUE,
                                             open_last = FALSE),
                 "'breaks' has length 0 but 'open_first' is TRUE")
    expect_error(err_tdy_breaks_integer_enum(breaks = 10,
                                             open_first = FALSE,
                                             open_last = FALSE),
                 "'breaks' has length 1 but 'open_first' and 'open_last' are both FALSE")
    expect_error(err_tdy_breaks_integer_enum(breaks = c(0L, NA),
                                             open_first = FALSE,
                                             open_last = FALSE),
                 "'breaks' has NAs")
    expect_error(err_tdy_breaks_integer_enum(breaks = c(0L, Inf),
                                             open_first = FALSE,
                                             open_last = FALSE),
                 "'breaks' has infinite values")
    expect_error(err_tdy_breaks_integer_enum(breaks = c(0L, 1.1),
                                             open_first = FALSE,
                                             open_last = FALSE),
                 "value '1.1' in 'breaks' not equivalent to integer")
    expect_error(err_tdy_breaks_integer_enum(breaks = c(1L, 0L),
                                             open_first = FALSE,
                                             open_last = FALSE),
                 "'breaks' is not strictly increasing : element 1 \\[1\\] is greater than or equal to element 2 \\[0\\]")
})    


## err_tdy_date_scalar

test_that("'err_tdy_date_scalar' returns dates with valid input", {
    x <- "2001-01-01"
    ans_obtained <- err_tdy_date_scalar(x = x, name = "x")
    ans_expected <- as.Date(x)
    expect_identical(ans_obtained, ans_expected)
    x <- as.Date("2001-01-01")
    ans_obtained <- err_tdy_date_scalar(x = x, name = "x")
    ans_expected <- as.Date(x)
    expect_identical(ans_obtained, ans_expected)
    x <- "2001/1/1"
    ans_obtained <- err_tdy_date_scalar(x = x, name = "x")
    ans_expected <- as.Date(x)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'err_tdy_date_scalar' raises expected error with invalid input", {
    expect_error(err_tdy_date_scalar(x = as.Date(c("2000-01-01", "2000-01-02")),
                                     name = "x"),
                 "'x' does not have length 1")
    expect_error(err_tdy_date_scalar(x = 1,
                                     name = "x"),
                 "'x' \\[\"1\"\\] not equivalent to date : 'origin' must be supplied")
    expect_error(err_tdy_date_scalar(x = "a",
                                     name = "x"),
                 "'x' \\[\"a\"\\] not equivalent to date : character string is not in a standard unambiguous format")
    expect_error(err_tdy_date_scalar(x = "a-02-01",
                                     name = "x"),
                 "'x' \\[\"a\\-02\\-01\"\\] not equivalent to date")
})


## err_tdy_date_vector

test_that("'err_tdy_date_vector' returns dates with valid input", {
    x <- c("2001-01-01", "2002-01-01")
    ans_obtained <- err_tdy_date_vector(x = x, name = "x")
    ans_expected <- as.Date(x)
    expect_identical(ans_obtained, ans_expected)
    x <- as.Date(c("2001-01-01", "2002-01-01"))
    ans_obtained <- err_tdy_date_vector(x = x, name = "x")
    ans_expected <- as.Date(x)
    expect_identical(ans_obtained, ans_expected)
    x <- c("2001/1/1", "2002/1/1")
    ans_obtained <- err_tdy_date_vector(x = x, name = "x")
    ans_expected <- as.Date(x)
    expect_identical(ans_obtained, ans_expected)
    x <- character()
    ans_obtained <- err_tdy_date_vector(x = x, name = "x")
    ans_expected <- as.Date(x)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'err_tdy_date_vector' raises expected error with invalid input", {
    expect_error(err_tdy_date_vector(x = 1,
                                     name = "x"),
                 "'x' \\[1\\] not equivalent to dates : 'origin' must be supplied")
    expect_error(err_tdy_date_vector(x = letters,
                                     name = "x"),
                 "'x' \\[a, b, c, \\.\\.\\.\\] not equivalent to dates : character string is not in a standard unambiguous format")
    expect_error(err_tdy_date_vector(x = c("2000-01-01", "a-02-01"),
                                     name = "x"),
                 "value \"a-02-01\" in 'x' not equivalent to date")
})


## err_tdy_date_dob

test_that("'err_tdy_date_dob' returns dates with valid input", {
    date <- as.Date(c("2001-01-01", "2002-01-01"))
    dob <- as.Date("2000-06-30")
    expect_identical(err_tdy_date_dob(date = date, dob = dob),
                     list(date = date,
                          dob = rep(dob, 2)))
})


## err_tdy_month_start

test_that("'err_tdy_month_start' returns day and month with valid input", {
    expect_identical(err_tdy_month_start("January"),
                     "Jan")
    expect_identical(err_tdy_month_start("Jan"),
                     "Jan")
    expect_identical(err_tdy_month_start("feb"),
                     "Feb")
    expect_identical(err_tdy_month_start("march"),
                     "Mar")
})

test_that("'err_tdy_month_start' raises expected error with invalid input", {
    expect_error(err_tdy_month_start(x = "wrong",
                                     name = "x"),
                 "invalid value for 'x' : \"wrong\"")
})
    

## err_tdy_integer_scalar

test_that("'err_tdy_integer_scalar' works with valid inputs", {
    expect_identical(err_tdy_integer_scalar(x = 1, name = "x"),
                     1L)
    expect_identical(err_tdy_integer_scalar(x = NA, name = "x"),
                     NA_integer_)
    expect_identical(err_tdy_integer_scalar(x = NULL, name = "x", null_ok = TRUE),
                     NULL)
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


## err_tdy_non_negative_integer_scalar

test_that("'err_tdy_non_negative_integer_scalar' works with valid inputs", {
    expect_identical(err_tdy_non_negative_integer_scalar(x = 1, name = "x"),
                     1L)
    expect_identical(err_tdy_integer_scalar(x = NULL, name = "x", null_ok = TRUE),
                     NULL)
    expect_identical(err_tdy_integer_scalar(x = 0L, name = "x", null_ok = TRUE),
                     0L)
})

test_that("'err_tdy_non_negative_integer_scalar' raises expected error with invalid input", {
    expect_error(err_tdy_non_negative_integer_scalar(x = 0.1,
                                                 name = "x"),
                 "'x' \\[0.1\\] not equivalent to integer")
    expect_error(err_tdy_non_negative_integer_scalar(x = NULL, name = "x", null_ok = FALSE),
                 "'x' is NULL")
})


## err_tdy_positive_integer_scalar

test_that("'err_tdy_positive_integer_scalar' works with valid inputs", {
    expect_identical(err_tdy_positive_integer_scalar(x = 1, name = "x"),
                     1L)
    expect_identical(err_tdy_integer_scalar(x = NULL, name = "x", null_ok = TRUE),
                     NULL)
})

test_that("'err_tdy_positive_integer_scalar' raises expected error with invalid input", {
    expect_error(err_tdy_positive_integer_scalar(x = 0.1,
                                                 name = "x"),
                 "'x' \\[0.1\\] not equivalent to integer")
    expect_error(err_tdy_positive_integer_scalar(x = NULL, name = "x"),
                 "'x' is NULL")
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

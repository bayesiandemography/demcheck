
context("chk-single")

## chk_is_date_equiv_scalar

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

## chk_is_date_equiv_vector

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


## chk_is_integer_equiv_vector

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

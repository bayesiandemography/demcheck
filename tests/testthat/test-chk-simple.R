
context("simple")

## chk_is_date_equiv

test_that("'chk_is_date_equiv' returns TRUE with valid dates", {
    expect_true(chk_is_date_equiv(x = as.Date(c("2001-01-01", "2002-01-01")),
                                  name = "x"))
    expect_true(chk_is_date_equiv(x = as.Date(character()),
                                  name = "x"))
})

test_that("'chk_is_date_equiv' returns expected message with invalid dates", {
    expect_identical(chk_is_date_equiv(x = 1,
                                       name = "x"),
                     "'x' [1] not equivalent to dates : 'origin' must be supplied")
    expect_identical(chk_is_date_equiv(x = letters,
                                       name = "x"),
                     "'x' [a, b, c, ...] not equivalent to dates : character string is not in a standard unambiguous format")
    expect_identical(chk_is_date_equiv(x = c("2000-01-01", "a-02-01"),
                                       name = "x"),
                     "value 'a-02-01' in 'x' not equivalent to date")
})



context("helper-functions")

## make_label -----------------------------------------------------------------

test_that("'make_label_intervals' gives expected result with valid inputs", {
    expect_identical(make_label_intervals(c(NA_integer_, NA_integer_)),
                     NA_character_)
    expect_identical(make_label_intervals(c(NA, 0L)),
                     "<0")
    expect_identical(make_label_intervals(c(100L, NA)),
                     "100+")
    expect_identical(make_label_intervals(c(20L, 21L)),
                     "20-21")
    expect_identical(make_label_intervals(c(20L, 24L)),
                     "20-24")
})

test_that("'make_label_quantities' gives expected result with valid inputs", {
    expect_identical(make_label_quantities(c(NA_integer_, NA_integer_)),
                     NA_character_)
    expect_identical(make_label_quantities(c(NA, -1L)),
                     "<0")
    expect_identical(make_label_quantities(c(100L, NA)),
                     "100+")
    expect_identical(make_label_quantities(c(20L, 20L)),
                     "20")
    expect_identical(make_label_quantities(c(20L, 24L)),
                     "20-24")
})

test_that("'make_label_quarters' gives expected result with valid inputs", {
    expect_identical(make_label_quarters(as.Date(c(NA_integer_, NA_integer_))),
                     NA_character_)
    expect_identical(make_label_quarters(as.Date(c(NA, "2020-07-01"))),
                     "<2020 Q3")
    expect_identical(make_label_quarters(as.Date(c("2020-07-01", NA))),
                     "2020 Q3+")
    expect_identical(make_label_quarters(as.Date(c("2020-04-01", "2020-07-01"))),
                     "2020 Q2")
})

test_that("'make_label_months' gives expected result with valid inputs", {
    expect_identical(make_label_months(as.Date(c(NA_integer_, NA_integer_))),
                     NA_character_)
    expect_identical(make_label_months(as.Date(c(NA, "2020-07-01"))),
                     "<2020 Jul")
    expect_identical(make_label_months(as.Date(c("2020-07-01", NA))),
                     "2020 Jul+")
    expect_identical(make_label_months(as.Date(c("2020-04-01", "2020-05-01"))),
                     "2020 Apr")
})

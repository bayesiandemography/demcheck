
context("chk-values")

## Categories -----------------------------------------------------------------

test_that("'chk_values_categories' gives expected result with valid inputs", {
  expect_true(chk_values_categories(c("A", "B"), name = "x"))
  expect_true(chk_values_categories(c("A", "B", NA, "C"), name = "x"))
  expect_true(chk_values_categories(character(), name = "x"))
})
  
test_that("'chk_values_triangles' gives expected result with valid inputs", {
  expect_true(chk_values_triangles(c("Upper", "Lower"), name = "x"))
  expect_true(chk_values_triangles(c("Lower", "Upper", NA), name = "x"))
  expect_true(chk_values_triangles(character(), name = "x"))
})

test_that("'chk_values_triangles' returns expected string with invalid inputs", {
  expect_identical(chk_values_triangles(c("Upper", "Lower", "Wrong"), "x"),
                   "'x' has unexpected value : \"Wrong\"")
})
  
test_that("'chk_values_directions' gives expected result with valid inputs", {
  expect_true(chk_values_directions(c("Out", "In"), name = "x"))
  expect_true(chk_values_directions(c("In", "Out", NA), name = "x"))
  expect_true(chk_values_directions(character(), name = "x"))
})

test_that("'chk_values_directions' returns expected string with invalid inputs", {
  expect_identical(chk_values_directions(c("In", "Out", "Wrong"), "x"),
                   "'x' has unexpected value : \"Wrong\"")
})

test_that("'chk_values_quantiles' gives expected result with valid inputs", {
  expect_true(chk_values_quantiles(c("75.22%", "0%"), name = "x"))
  expect_true(chk_values_quantiles(c("21.33%", "100%", NA), name = "x"))
  expect_true(chk_values_quantiles(character(), name = "x"))
})


## Pairs ----------------------------------------------------------------------

test_that("'chk_values_pairs' gives expected result with valid inputs", {
    expect_true(chk_values_pairs(list(c(2000L, 2001L), c(0L, 5L), c(NA_integer_, NA_integer_),
                                      c(2005L, NA), c(NA, 0L)),
                                 name = "x"))
    expect_true(chk_values_pairs(list(as.Date(c("2000-05-01", "2000-09-01"))),
                                 name = "x"))
    expect_true(chk_values_pairs(list(c(NA_integer_, NA_integer_)),
                                 name = "x"))
    expect_true(chk_values_pairs(list(),
                                 name = "x"))
})

test_that("'chk_values_integers' gives expected result with valid inputs", {
    expect_true(chk_values_integers(list(c(2000L, 2001L),
                                         c(0L, 1L),
                                         c(NA_integer_, NA_integer_),
                                         c(2005L, NA),
                                         c(NA, 0L)),
                                    name = "x"))
    expect_true(chk_values_integers(list(), name = "x"))
})

test_that("'chk_values_intervals' gives expected result with valid inputs", {
    expect_true(chk_values_intervals(list(c(2000L, 2100L),
                                          c(0L, 1L),
                                          c(NA_integer_, NA_integer_),
                                          c(2105L, NA),
                                          c(NA, 0L)),
                                     name = "x"))
    expect_true(chk_values_intervals(list(), name = "x"))
})

test_that("'chk_values_quantities' gives expected result with valid inputs", {
    expect_true(chk_values_quantities(list(c(2000L, 2100L),
                                           c(0L, 1L),
                                           c(NA_integer_, NA_integer_),
                                           c(2105L, NA),
                                           c(NA, 0L)),
                                      name = "x"))
    expect_true(chk_values_quantities(list(),
                                      name = "x"))
})

test_that("'chk_values_quarters' gives expected result with valid inputs", {
    expect_true(chk_values_quarters(list(as.Date(c("2020-04-01", "2020-07-01")),
                                         as.Date(c(NA, NA)),
                                         as.Date(c("2025-10-01", NA)),
                                         as.Date(c(NA, "2000-01-01"))),
                                    name = "x"))
    expect_true(chk_values_quarters(list(),
                                    name = "x"))
})

test_that("'chk_values_months' gives expected result with valid inputs", {
    expect_true(chk_values_months(list(as.Date(c("2020-04-01", "2020-05-01")),
                                       as.Date(c(NA, NA)),
                                       as.Date(c("2025-10-01", NA)),
                                       as.Date(c(NA, "2000-01-01"))),
                                  name = "x"))
    expect_true(chk_values_months(list(), name = "x"))
})

test_that("'chk_values_dateranges' gives expected result with valid inputs", {
    expect_true(chk_values_dateranges(list(as.Date(c("2020-04-02", "2020-05-13")),
                                           as.Date(c(NA, NA)),
                                           as.Date(c("2025-10-31", NA)),
                                           as.Date(c(NA, "2000-02-29"))),
                                      name = "x"))
    expect_true(chk_values_dateranges(list()))
})


## DatePoints ----------------------------------------------------------------------

test_that("'chk_values_datepoints' gives expected result with valid inputs", {
    expect_true(chk_values_datepoints(as.Date(c("2000-11-03", NA, "1990-01-22")),
                                      name = "x"))
    expect_true(chk_values_datepoints(as.Date(NA), name = "x"))
    expect_true(chk_values_datepoints(as.Date(character(), name = "x")))
})

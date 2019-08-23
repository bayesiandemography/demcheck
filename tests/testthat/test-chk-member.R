
context("chk-member")

## chk_member_unit

test_that("'chk_member_unit' returns TRUE with time units", {
    expect_true(chk_member_unit(x = "month",
                                     name = "x"))
    expect_true(chk_member_unit(x = "quarter",
                                     name = "x"))
})

test_that("'chk_member_unit' returns expected message with invalid time units", {
    expect_identical(chk_member_unit(x = "year",
                                          name = "x"),
                     "value for 'x' [\"year\"] is not a permitted time unit")
})

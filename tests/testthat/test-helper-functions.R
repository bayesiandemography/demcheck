
## make_label_intervals -------------------------------------------------------

test_that("'make_label_intervals' gives expected result with valid inputs", {
    expect_identical(make_label_intervals(c(NA, 0L)),
                     "--0")
    expect_identical(make_label_intervals(c(100L, NA)),
                     "100--")
    expect_identical(make_label_intervals(c(20L, 21L)),
                     "20--21")
    expect_identical(make_label_intervals(c(20L, 24L)),
                     "20--24")
})


## make_label_quantities ------------------------------------------------------

test_that("'make_label_intervals' gives expected result with valid inputs", {
    expect_identical(make_label_quantities(c(NA, -1L)),
                     "<=-1")
    expect_identical(make_label_quantities(c(100L, NA)),
                     "100+")
    expect_identical(make_label_quantities(c(20L, 20L)),
                     "20")
    expect_identical(make_label_quantities(c(20L, 24L)),
                     "20-24")
})


## chk_map_dim ------------------------------------------------------------

test_that("'chk_map_dim' works with valid input", {
    expect_true(chk_map_dim(x = c(3L, 0L, 1L), name = "x"))
})

test_that("'chk_map_dim' works with 'map_dim' of length 1", {
    expect_true(chk_map_dim(x = 2L, name = "x"))
})

test_that("'chk_map_dim' works with zero element", {
    expect_true(chk_map_dim(x = 0:1, name = "x"))
})


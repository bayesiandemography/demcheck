
## chk_map_dim ------------------------------------------------------------

test_that("'chk_map_dim' works with valid input", {
    map_dim <- c(1L, 3L, 2L, 0L)
    dim_self <- 1:4
    dim_oth <- c(1L, 2L, 2L)
    expect_true(chk_map_dim(map_dim = map_dim,
                                    dim_self = dim_self,
                                    dim_oth = dim_oth))
})

test_that("'chk_map_dim' works with 'dim_self' of length 1", {
    map_dim <- 2L
    dim_self <- 2L
    dim_oth <- c(1L, 2L)
    expect_true(chk_map_dim(map_dim = map_dim,
                                    dim_self = dim_self,
                                    dim_oth = dim_oth))
})

test_that("'chk_map_dim' works with 'dim_oth' of length 1", {
    map_dim <- c(0L, 1L)
    dim_self <- 3:4
    dim_oth <- 4L
    expect_true(chk_map_dim(map_dim = map_dim,
                            dim_self = dim_self,
                            dim_oth = dim_oth))
})



context("chk-specific")

## chk_comp_type_indices --------------------------------------------------

test_that("'chk_comp_type_indices' works with valid input", {
    expect_true(chk_comp_type_indices(i_comp_type_self = 1L,
                                      indices_orig_self = 0L,
                                      i_direction_self = 0L))
    expect_true(chk_comp_type_indices(i_comp_type_self = 3L,
                                      indices_orig_self = 1:2,
                                      i_direction_self = 0L))
    expect_true(chk_comp_type_indices(i_comp_type_self = 4L,
                                      indices_orig_self = 0L,
                                      i_direction_self = 1L))
})

test_that("'chk_comp_type_indices' returns expected message with invalid inputs", {
    expect_identical(chk_comp_type_indices(i_comp_type_self = 3L,
                                           indices_orig_self = 0L,
                                           i_direction_self = 0L),
                     "'i_comp_type_self' is 3 but 'indices_orig_self' is 0")
    expect_identical(chk_comp_type_indices(i_comp_type_self = 4L,
                                           indices_orig_self = 1L,
                                           i_direction_self = 0L),
                     "'i_comp_type_self' is 4 but 'indices_orig_self' is not 0")
    expect_identical(chk_comp_type_indices(i_comp_type_self = 4L,
                                           indices_orig_self = 0L,
                                           i_direction_self = 0L),
                     "'i_comp_type_self' is 4 but 'i_direction_self' is 0")
    expect_identical(chk_comp_type_indices(i_comp_type_self = 1L,
                                           indices_orig_self = 0L,
                                           i_direction_self = 5L),
                     "'i_comp_type_self' is 1 but 'i_direction_self' is not 0")
})


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


## chk_map_pos ------------------------------------------------------------

test_that("'chk_map_dim' works with valid input", {
    expect_true(chk_map_pos(x = list(c(3L, 0L, 1L), 1:2), name = "x"))
})

test_that("'chk_map_dim' returns expected message with invalid inputs", {
    expect_identical(chk_map_pos(x = list(1:2, integer()),
                                 name = "x"),
                     "'element 2 of 'x'' has length 0")
})


## chk_no_open_age ------------------------------------------------------------

test_that("'chk_no_open_age' works with valid input", {
    expect_true(chk_no_open_age(x = c("0", "77", "50-90", NA)))
    expect_true(chk_no_open_age(x = character()))
})
    
test_that("'chk_no_open_age' returns expected message with invalid inputs", {
    expect_identical(chk_no_open_age(x = c("0", "77", "100+", "50-90", NA)),
                     "'open_last' is FALSE but age group \"100+\" is open")    
})


## chk_no_open_cohort ---------------------------------------------------------

test_that("'chk_no_open_cohort' works with valid input", {
    expect_true(chk_no_open_cohort(x = c("2000", "2077", "1950-1990", NA)))
    expect_true(chk_no_open_cohort(x = character()))
})
    
test_that("'chk_no_open_age' returns expected message with invalid inputs", {
    expect_identical(chk_no_open_cohort(x = c("<2000", "2077", "1950-1990", NA)),
                     "'open_first' is FALSE but cohort \"<2000\" is open")    
    expect_identical(chk_no_open_cohort(x = c("2000", "2077", "<-500", NA)),
                     "'open_first' is FALSE but cohort \"<-500\" is open")    
})


## chk_open_left_le_break_min -------------------------------------------------

test_that("'chk_open_left_le_break_min' works with valid input", {
    expect_true(chk_open_left_le_break_min(labels = c("<2000", "2077", "1950-1990", NA),
                                           int_up = c(2000L, 2078L, 1990L, NA),
                                           is_open = c(TRUE, FALSE, FALSE, FALSE),
                                           break_min = 2000L))
    expect_true(chk_open_left_le_break_min(labels = character(),
                                           int_up = integer(),
                                           is_open = logical(),
                                           break_min = 2000L))
    expect_true(chk_open_left_le_break_min(labels = c("<2005", "2077", "1950-1990", NA),
                                           int_up = c(2005L, 2078L, 1990L, NA),
                                           is_open = c(TRUE, FALSE, FALSE, FALSE),
                                           break_min = NULL))
})
    
test_that("'chk_open_left_le_break_min' returns expected message with invalid inputs", {
    expect_identical(chk_open_left_le_break_min(labels = c("<2005", "2077", "1950-1990", NA),
                                                int_up = c(2005L, 2078L, 1990L, NA),
                                                is_open = c(TRUE, FALSE, FALSE, FALSE),
                                                break_min = 2000L),
                     "upper limit of open interval \"<2005\" is greater than 'break_min' [2000]")
})


## chk_overlap_pairs ----------------------------------------------------------

test_that("'chk_no_overlap_pairs' works with valid input", {
    expect_true(chk_no_overlap_pairs(x = list(c(NA, 2000L),
                                              c(2000L, 2001L),
                                              c(2001L, 2005L),
                                              c(2005L, 2010L),
                                              c(NA_integer_, NA_integer_),
                                              c(2010L, NA)),
                                     name = "x"))
    expect_true(chk_no_overlap_pairs(x = list(c(2000L, 2001L),
                                              c(2005L, 2010L),
                                              c(NA_integer_, NA_integer_),
                                              c(2001L, 2005L),
                                              c(NA, 2000L),
                                              c(2010L, NA)),
                                     name = "x"))
    expect_true(chk_no_overlap_pairs(x = list(c(0L, 1L)),
                                     name = "x"))
    expect_true(chk_no_overlap_pairs(x = list(),
                                     name = "x"))
    expect_true(chk_no_overlap_pairs(x = list(c(NA_integer_, NA_integer_)),
                                     name = "x"))
})
    
test_that("'chk_no_overlap_pairs' returns expected message with invalid inputs", {
    expect_identical(chk_no_overlap_pairs(x = list(c(NA, 2000L),
                                                   c(2000L, 2002L),
                                                   c(NA_integer_, NA_integer_),
                                                   c(2001L, 2005L),
                                                   c(2005L, 2010L),
                                                   c(2010L, NA)),
                                          name = "x"),
                     "problem with elements of 'x' : \"2000-2002\" overlaps with \"2001-2005\"")
    expect_identical(chk_no_overlap_pairs(x = list(c(0L, 1L),
                                                   c(NA_integer_, NA_integer_),
                                                   c(0L, 1L)),
                                          name = "x"),
                     "problem with elements of 'x' : \"0-1\" overlaps with \"0-1\"")
    expect_identical(chk_no_overlap_pairs(x = list(c(5L, NA),
                                                   c(NA_integer_, NA_integer_),
                                                   c(0L, NA)),
                                          name = "x"),
                     "problem with elements of 'x' : \"0+\" overlaps with \"5+\"")
    expect_identical(chk_no_overlap_pairs(x = list(c(NA, 1L),
                                                   c(NA_integer_, NA_integer_),
                                                   c(0L, 1L)),
                                          name = "x"),
                     "problem with elements of 'x' : \"<1\" overlaps with \"0-1\"")
    expect_identical(chk_no_overlap_pairs(x = list(c(0L, 0L),
                                                   c(10L, NA),
                                                   c(NA_integer_, NA_integer_),
                                                   c(1L, 5L),
                                                   c(NA, -1L),
                                                   c(4L, 9L)),
                                          name = "x"),
                     "problem with elements of 'x' : \"1-5\" overlaps with \"4-9\"")
    expect_identical(chk_no_overlap_pairs(x = list(c(0L, 0L),
                                                   c(10L, NA),
                                                   c(NA_integer_, NA_integer_),
                                                   c(NA_integer_, NA_integer_),
                                                   c(1L, 5L),
                                                   c(NA, -1L)),
                                          name = "x"),
                     "problem with elements of 'x' : more than one item where both elements are NA")
})


## chk_overlap_months ---------------------------------------------------------

test_that("'chk_no_overlap_months' works with valid input", {
    expect_true(chk_no_overlap_months(x = list(as.Date(c(NA, "2000-01-01")),
                                               as.Date(c("2000-02-01", NA)),
                                               as.Date(c("2000-01-01", "2000-02-01")),
                                               as.Date(c(NA, NA))),
                                      name = "x"))
    expect_true(chk_no_overlap_months(x = list(as.Date(c("2000-02-01", NA))),
                                      name = "x"))
    expect_true(chk_no_overlap_months(x = list(),
                                      name = "x"))
    expect_true(chk_no_overlap_months(x = list(as.Date(c(NA, NA))),
                                      name = "x"))
})
    
test_that("'chk_no_overlap_months' returns expected message with invalid inputs", {
    expect_identical(chk_no_overlap_months(x = list(as.Date(c(NA, "2000-01-01")),
                                                    as.Date(c("2000-01-01", NA)),
                                                    as.Date(c("2000-01-01", "2000-02-01")),
                                                    as.Date(c(NA, NA))),
                                           name = "x"),
                     "problem with elements of 'x' : \"2000 Jan\" overlaps with \"2000 Jan+\"")
    expect_identical(chk_no_overlap_months(x = list(as.Date(c(NA, "2000-01-01")),
                                                    as.Date(c("2000-01-01", "2000-02-01")),
                                                    as.Date(c("2000-01-01", "2000-02-01")),
                                                    as.Date(c(NA, NA))),
                                           name = "x"),
                     "problem with elements of 'x' : \"2000 Jan\" overlaps with \"2000 Jan\"")
    expect_identical(chk_no_overlap_months(x = list(as.Date(c("2000-01-01", NA)),
                                                    as.Date(c(NA, NA)),
                                                    as.Date(c(NA, NA))),
                                              name = "x"),
                     "problem with elements of 'x' : more than one item where both elements are NA")
})


## chk_overlap_quarters -------------------------------------------------------

test_that("'chk_no_overlap_quarters' works with valid input", {
    expect_true(chk_no_overlap_quarters(x = list(as.Date(c(NA, "2000-01-01")),
                                               as.Date(c("2000-04-01", NA)),
                                               as.Date(c("2000-01-01", "2000-04-01")),
                                               as.Date(c(NA, NA))),
                                      name = "x"))
    expect_true(chk_no_overlap_quarters(x = list(as.Date(c("2000-04-01", NA))),
                                      name = "x"))
    expect_true(chk_no_overlap_quarters(x = list(),
                                      name = "x"))
    expect_true(chk_no_overlap_quarters(x = list(as.Date(c(NA, NA))),
                                      name = "x"))
})
    
test_that("'chk_no_overlap_quarters' returns expected message with invalid inputs", {
    expect_identical(chk_no_overlap_quarters(x = list(as.Date(c(NA, "2000-01-01")),
                                                      as.Date(c("2000-01-01", NA)),
                                                      as.Date(c("2000-01-01", "2000-04-01")),
                                                      as.Date(c(NA, NA))),
                                             name = "x"),
                     "problem with elements of 'x' : \"2000 Q1\" overlaps with \"2000 Q1+\"")
    expect_identical(chk_no_overlap_quarters(x = list(as.Date(c(NA, "2000-01-01")),
                                                      as.Date(c("2000-01-01", "2000-04-01")),
                                                      as.Date(c("2000-01-01", "2000-04-01")),
                                                      as.Date(c(NA, NA))),
                                             name = "x"),
                     "problem with elements of 'x' : \"2000 Q1\" overlaps with \"2000 Q1\"")
    expect_identical(chk_no_overlap_quarters(x = list(as.Date(c("2000-01-01", NA)),
                                                      as.Date(c(NA, NA)),
                                                      as.Date(c(NA, NA))),
                                             name = "x"),
                     "problem with elements of 'x' : more than one item where both elements are NA")
})


## chk_open_right_ge_break_max ------------------------------------------------

test_that("'chk_open_right_ge_break_max' works with valid input", {
    expect_true(chk_open_right_ge_break_max(labels = c("2100+", "2077", "1950-1990", NA),
                                            int_low = c(2100L, 2077L, 1950L, NA),
                                            is_open = c(TRUE, FALSE, FALSE, FALSE),
                                            break_max = 2100L))
    expect_true(chk_open_right_ge_break_max(labels = character(),
                                            int_low = integer(),
                                            is_open = logical(),
                                            break_max = 2000L))
    expect_true(chk_open_right_ge_break_max(labels = c("2095+", "2077", "1950-1990", NA),
                                                 int_low = c(2095L, 2077L, 1950L, NA),
                                                 is_open = c(TRUE, FALSE, FALSE, FALSE),
                                                 break_max = NULL))
})
    
test_that("'chk_open_right_ge_break_max' returns expected message with invalid inputs", {
    expect_identical(chk_open_right_ge_break_max(labels = c("2095+", "2077", "1950-1990", NA),
                                                 int_low = c(2095L, 2077L, 1950L, NA),
                                                 is_open = c(TRUE, FALSE, FALSE, FALSE),
                                                 break_max = 2100L),
                     "lower limit of open interval \"2095+\" is less than 'break_max' [2100]")
})

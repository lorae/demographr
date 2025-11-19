library("testthat")
library("readr")
library("rlang")
library("rprojroot")
library("tibble")
library("dplyr")
library("purrr")

# Set working directory to project root
root <- find_root(is_rstudio_project)
setwd(root)

# Load the package 
devtools::load_all(".")

# Read test fixture
hh_single_mom <- tibble(
  id         = c(1, 2, 3),
  age        = c(30, 7, 9),
  sex        = c(2, 1, 1),
  mother_id  = c(NA, 1, 1),
  father_id  = c(NA, NA, NA),
  spouse_id  = c(NA, NA, NA),
  is_hoh     = c(1, 0, 0)
)

hh_grandfamily <- tibble(
  id         = c(1, 2, 3, 4, 5, 6, 7),
  age        = c(65, 15, 34, 1, 13, 17, 41),
  sex        = c(2, 2, 2, 1, 1, 2, 1),
  mother_id  = c(NA, 1, 1, 3, 3, 3, NA),
  father_id  = c(NA, NA, NA, 7, 7, 7, NA),
  spouse_id  = c(NA, NA, 7, NA, NA, NA, 3),
  is_hoh     = c(1, 0, 0, 0, 0, 0, 0)
)

hh_four <- tibble(
  id         = c(1, 2, 3, 4),
  age        = c(30, 60, 61, 32),
  sex        = c(2, 2, 1, 1),
  mother_id  = c(2, NA, NA, NA),
  father_id  = c(3, NA, NA, NA),
  spouse_id  = c(4, 3, 2, 1),
  is_hoh     = c(1, 0, 0, 0)
)

# ----- Unit tests ----- #

test_that("household_adjacency builds edges correctly in 3 cases", {
  
  expected_single_mom <- matrix(
    c(
      0, 1, 1,
      0, 0, 0,
      0, 0, 0
    ),
    nrow = 3,
    byrow = TRUE
  )
  
  expected_grandfamily <- matrix(
    c(
      #1 2 3 4 5 6 7
       0,1,1,0,0,0,0,   # id 1 
       0,0,0,0,0,0,0,   # id 2
       0,0,0,1,1,1,1,   # id 3 
       0,0,0,0,0,0,0,   # id 4
       0,0,0,0,0,0,0,   # id 5
       0,0,0,0,0,0,0,   # id 6
       0,0,1,1,1,1,0    # id 7
    ),
    nrow = 7,
    byrow = TRUE
  )
  
  expected_four <- matrix(
    c(
      0,0,0,1,
      1,0,1,0,
      1,1,0,0,
      1,0,0,0
    ),
    nrow = 4,
    byrow = TRUE
  )
  
  output_single_mom <- household_adjacency(hh_single_mom)
  output_grandfamily <- household_adjacency(hh_grandfamily)
  output_four <- household_adjacency(hh_four)
  
  # Test
  expect_equal(output_single_mom, expected_single_mom)
  expect_equal(output_grandfamily, expected_grandfamily)
  expect_equal(output_four, expected_four)
})test_that("household_adjacency warns on non-zero diagonal", {
  # Create fake hh where an impossible self-edge is forced
  hh <- tibble(
    id = c(1, 2),
    age = c(40, 10),
    mother_id = c(1, 1),   # person 1 listed as mother of self
    father_id = c(0, 0),
    spouse_id = c(NA, NA)
  )
  
  expect_warning(
    household_adjacency(hh),
    regexp = "non-zero diagonal"
  )
})

test_that("household_adjacency warns on entries not 0 or 1", {
  # Build a case where your lower-level functions double-count
  # (simulate by manually adding a duplicate edge after generation)
  hh <- tibble(
    id = c(1, 2),
    age = c(40, 10),
    mother_id = c(NA, 1),
    father_id = c(NA, 1),  # double parent -> will produce sum = 2
    spouse_id = c(NA, NA)
  )
  
  expect_warning(
    household_adjacency(hh),
    regexp = "values other than 0 or 1"
  )
})

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

# ----- Unit tests ----- #

test_that("parent_adjacency builds mother-child edges correctly in household with two edges", {
  
  # Expected output
  expected <- matrix(
    c(
      0, 1, 1,
      0, 0, 0,
      0, 0, 0
    ),
    nrow = 3,
    byrow = TRUE
  )
  
  # Run adjacency
  output <- parent_adjacency(hh_single_mom, parent_col = "mother_id")
  
  # Test
  expect_equal(output, expected)
})

test_that("parent_adjacency returns all zeros when father_id has no valid links", {
  
  # Expected: 3×3 matrix of zeros
  expected <- matrix(
    c(
      0, 0, 0,
      0, 0, 0,
      0, 0, 0
    ),
    nrow = 3,
    byrow = TRUE
  )
  
  output <- parent_adjacency(hh_single_mom, parent_col = "father_id")
  
  expect_equal(output, expected)
})


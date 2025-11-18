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

test_that("spouse_adjacency builds spousal edges correctly in household with no edges", {
  
  expected <- matrix(
    c(
      0, 0, 0,
      0, 0, 0,
      0, 0, 0
    ),
    nrow = 3,
    byrow = TRUE
  )
  
  output <- spouse_adjacency(hh_single_mom)
  
  # Test
  expect_equal(output, expected)
})

test_that("spouse_adjacency builds spousal edges correctly in household with one edge", {
  
  expected <- matrix(
    c(
      # 1 2 3 4 5 6 7
        0,0,0,0,0,0,0,   # id 1 → 2,3
        0,0,0,0,0,0,0,   # id 2
        0,0,0,0,0,0,1,   # id 3 → 4,5,6
        0,0,0,0,0,0,0,   # id 4
        0,0,0,0,0,0,0,   # id 5
        0,0,0,0,0,0,0,   # id 6
        0,0,1,0,0,0,0    # id 7
    ),
    nrow = 7,
    byrow = TRUE
  )
  
  output <- spouse_adjacency(hh_grandfamily)
  
  # Test
  expect_equal(output, expected)
})

test_that("spouse_adjacency builds spousal edges correctly in household with two edges", {
  
  expected <- matrix(
    c(
      #1 2 3 4
       0,0,0,1,   # 1 → 4
       0,0,1,0,
       0,1,0,0,
       1,0,0,0    # 4 → 1 (added automatically)
    ),
    nrow = 4,
    byrow = TRUE
  )
  
  output <- spouse_adjacency(hh_four)
  
  # Test
  expect_equal(output, expected)
})

test_that("spouse_adjacency handles asymmetric spouse listings correctly", {
  
  input <- hh_four |>
    mutate(spouse_id = c(4,3, NA, NA))
    
  expected <- matrix(
    c(
      #1 2 3 4
      0,0,0,1,   # 1 → 4
      0,0,1,0,
      0,1,0,0,
      1,0,0,0    # 4 → 1 (added automatically)
    ),
    nrow = 4,
    byrow = TRUE
  )
  
  output <- spouse_adjacency(input)
  
  # Test
  expect_equal(output, expected)
})
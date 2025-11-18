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

test_that("parent_adjacency builds correct mother-child edges for hh_grandfamily", {
  
  expected <- matrix(
    c(
      # 1 2 3 4 5 6 7
      0,1,1,0,0,0,0,   # id 1 → 2,3
      0,0,0,0,0,0,0,   # id 2
      0,0,0,1,1,1,0,   # id 3 → 4,5,6
      0,0,0,0,0,0,0,   # id 4
      0,0,0,0,0,0,0,   # id 5
      0,0,0,0,0,0,0,   # id 6
      0,0,0,0,0,0,0    # id 7
    ),
    nrow = 7,
    byrow = TRUE
  )
  
  output <- parent_adjacency(hh_grandfamily, parent_col = "mother_id")
  
  expect_equal(output, expected)
})

test_that("parent_adjacency builds correct father-child edges for hh_grandfamily", {
  
  expected <- matrix(
    c(
      # 1 2 3 4 5 6 7
      0,0,0,0,0,0,0,   # id 1
      0,0,0,0,0,0,0,   # id 2
      0,0,0,0,0,0,0,   # id 3
      0,0,0,0,0,0,0,   # id 4
      0,0,0,0,0,0,0,   # id 5
      0,0,0,0,0,0,0,   # id 6
      0,0,0,1,1,1,0    # id 7 → 4,5,6
    ),
    nrow = 7,
    byrow = TRUE
  )
  
  output <- parent_adjacency(hh_grandfamily, parent_col = "father_id")
  
  expect_equal(output, expected)
})

test_that("parent_adjacency handles mixed NA and 0 mother_id entries correctly", {
  
  hh_mixed <- hh_grandfamily |> 
    mutate(mother_id = c(0, 1, 1, 3, 3, 3, NA))
  
  expected <- matrix(
    c(
      #1 2 3 4 5 6 7
      0,1,1,0,0,0,0,   # id 1
      0,0,0,0,0,0,0,   # id 2
      0,0,0,1,1,1,0,   # id 3
      0,0,0,0,0,0,0,   # id 4
      0,0,0,0,0,0,0,   # id 5
      0,0,0,0,0,0,0,   # id 6
      0,0,0,0,0,0,0    # id 7
    ),
    nrow = 7,
    byrow = TRUE
  )
  
  output <- parent_adjacency(hh_mixed, parent_col = "mother_id")
  
  expect_equal(output, expected)
})

test_that("parent_adjacency treats 0 the same as NA for missing mother_id", {
  
  hh_zeroed <- hh_grandfamily |> 
    mutate(mother_id = c(0, 1, 1, 3, 3, 3, 0))
  
  expected <- matrix(
    c(
      #1 2 3 4 5 6 7
      0,1,1,0,0,0,0,   # id 1
      0,0,0,0,0,0,0,   # id 2
      0,0,0,1,1,1,0,   # id 3
      0,0,0,0,0,0,0,   # id 4
      0,0,0,0,0,0,0,   # id 5
      0,0,0,0,0,0,0,   # id 6
      0,0,0,0,0,0,0    # id 7
    ),
    nrow = 7,
    byrow = TRUE
  )
  
  output <- parent_adjacency(hh_zeroed, parent_col = "mother_id")
  
  expect_equal(output, expected)
})

test_that("parent_adjacency respects max_age = 22 in hh_grandfamily", {
  
  expected <- matrix(
    c(
      #1 2 3 4 5 6 7
      0,1,0,0,0,0,0,   # id 1 → child 2 only (child 3 is age 34 > 22)
      0,0,0,0,0,0,0,   # id 2
      0,0,0,1,1,1,0,   # id 3 → children 4,5,6 (all ≤ 22)
      0,0,0,0,0,0,0,   # id 4
      0,0,0,0,0,0,0,   # id 5
      0,0,0,0,0,0,0,   # id 6
      0,0,0,0,0,0,0    # id 7
    ),
    nrow = 7,
    byrow = TRUE
  )
  
  output <- parent_adjacency(
    hh_grandfamily,
    parent_col = "mother_id",
    max_age = 22
  )
  
  expect_equal(output, expected)
})

test_that("parent_adjacency is invariant to row order in hh_grandfamily", {
  
  # Scramble the row order
  set.seed(123)
  hh_scrambled <- hh_grandfamily |> slice(sample(n()))
  
  # Print both for visual confirmation
  print("Original hh_grandfamily:")
  print(hh_grandfamily)
  
  print("Scrambled hh_grandfamily:")
  print(hh_scrambled)
  
  # Ensure the scrambled version is different from the original
  expect_false(identical(hh_scrambled, hh_grandfamily))
  
  # Expected adjacency from canonical ordering
  expected <- matrix(
    c(
      #1 2 3 4 5 6 7
      0,1,1,0,0,0,0,   # id 1
      0,0,0,0,0,0,0,   # id 2
      0,0,0,1,1,1,0,   # id 3
      0,0,0,0,0,0,0,   # id 4
      0,0,0,0,0,0,0,   # id 5
      0,0,0,0,0,0,0,   # id 6
      0,0,0,0,0,0,0    # id 7
    ),
    nrow = 7,
    byrow = TRUE
  )
  
  output <- parent_adjacency(hh_scrambled, parent_col = "mother_id")
  
  expect_equal(output, expected)
})



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
mat_roommates <- matrix(
    c(
      0, 0,
      0, 0
    ),
    nrow = 2,
    byrow = TRUE
  )

mat_grandfamily <- matrix(
    c(
      #1 2 3 4 5 6 7
      0,1,0,0,0,0,0,   # id 1 
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

mat_single_mom <- matrix(
  c(
    0, 1, 1,
    0, 0, 0,
    0, 0, 0
  ),
  nrow = 3,
  byrow = TRUE
)


# ----- Unit tests ----- #
test_that("count_components works across household adjacency scenarios", {
  
  # -------------------------
  # 1. Single-mom household
  # -------------------------
  comps_single <- count_components(mat_single_mom)
  
  expect_equal(comps_single$membership, c(1, 1, 1))
  expect_equal(comps_single$csize, 3)
  expect_equal(comps_single$no, 1)
  
  
  # -------------------------
  # 2. Roommates (no edges)
  # -------------------------
  comps_roommates <- count_components(mat_roommates)
  
  expect_equal(comps_roommates$membership, c(1, 2))
  expect_equal(sort(comps_roommates$csize), c(1, 1))
  expect_equal(comps_roommates$no, 2)
  
  
  # -------------------------
  # 3. Grandfamily cluster
  # -------------------------
  comps_grand <- count_components(mat_grandfamily)
  
  expect_equal(comps_grand$membership, c(1, 1, 2, 2, 2, 2, 2))
  expect_equal(comps_grand$csize, c(2, 5))
  expect_equal(comps_grand$no, 2)
  
})

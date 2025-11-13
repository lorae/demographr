# household-viz.R
# The purpose of this file is to define functions related to producing and visualizing 
# household-level network graphs

# Convert a household level table to a network graph
# the tables will look like what is in tests/test-data/household-fixtures/*.csv
library(readr)
library(igraph)
library(tibble)

test_hh <- read_csv("tests/test-data/household-fixtures/single_mom_3_kids.csv")

hh <- tibble(
  id         = c(1, 2, 3),
  age        = c(30, 7, 9),
  sex        = c(2, 1, 1),
  mother_id  = c(NA, 1, 1),
  father_id  = c(NA, NA, NA),
  spouse_id  = c(NA, NA, NA),
  relate     = c("HOH", "Child", "Child")
)

hh


# 1 is the mother of 2 and 3, so edges are 1 -> 2 and 1 -> 3
edges <- matrix(
  c(1, 2,
    1, 3),
  ncol = 2,
  byrow = TRUE
)

g <- igraph::graph_from_edgelist(edges, directed = TRUE)

# Attach vertex attributes from hh
V(g)$id     <- hh$id
V(g)$age    <- hh$age
V(g)$sex    <- hh$sex
V(g)$relate <- hh$relate
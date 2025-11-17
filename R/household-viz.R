# household-viz.R
# The purpose of this file is to define functions related to producing and visualizing 
# household-level network graphs

# Convert a household level table to a network graph
# the tables will look like what is in tests/test-data/household-fixtures/*.csv
library(readr)
library(igraph)
library(tibble)
library(dplyr)

test_hh <- read_csv("tests/test-data/household-fixtures/single_mom_3_kids.csv")

# ----- Step 1: Define some example households ----- #
hh1 <- tibble(
  id         = c(1, 2, 3),
  age        = c(30, 7, 9),
  sex        = c(2, 1, 1),
  mother_id  = c(NA, 1, 1),
  father_id  = c(NA, NA, NA),
  spouse_id  = c(NA, NA, NA),
  is_hoh     = c(1, 0, 0)
)

hh1

hh2 <- tibble(
  id         = c(1, 2, 3, 4, 5, 6, 7),
  age        = c(65, 15, 34, 1, 13, 17, 41),
  sex        = c(2, 2, 2, 1, 1, 2, 1),
  mother_id  = c(NA, 1, 1, 3, 3, 3, NA),
  father_id  = c(NA, NA, NA, 7, 7, 7, NA),
  spouse_id  = c(NA, NA, 7, NA, NA, NA, 3),
  is_hoh = c(1, 0, 0, 0, 0, 0, 0)
)

hh2

# Note: the adjacency matrix in this function points from row -> col
# e.g. 
#    1   2
# 1  0   1
# 2  0   0
# This matrix has one directed edge, pointing from person 1 to person 2
household_to_adjacency <- function(hh_tibble) {
  # Count number of household members
  m <- nrow(hh_tibble) 
  
  # Initialize an adjacency matrix without edges
  mat <- matrix(0, nrow = m, ncol = m) 
  
  # First do the directed (mother -> child) edges
  mat
}

household_to_adjacency(hh1)
household_to_adjacency(hh2)

household_to_graph <- function(hh) {
  
  # ----- Parent–child edges -----
  mother_edges <- hh |>
    filter(!is.na(mother_id)) |>
    transmute(from = mother_id, to = id)
  
  father_edges <- hh |>
    filter(!is.na(father_id)) |>
    transmute(from = father_id, to = id)
  
  # ----- Spouse edges -----
  spouse_edges <- hh |>
    filter(!is.na(spouse_id), id < spouse_id) |>
    transmute(from = id, to = spouse_id)
  
  # ----- Combine all edges -----
  edge_df <- bind_rows(mother_edges, father_edges, spouse_edges)
  
  # ----- Build graph -----
  if (nrow(edge_df) == 0) {
    g <- igraph::make_empty_graph(n = nrow(hh), directed = FALSE)
    V(g)$name <- as.character(hh$id)
  } else {
    g <- igraph::graph_from_data_frame(
      d = edge_df,
      directed = FALSE,
      vertices = data.frame(name = hh$id)
    )
  }
  
  # ----- Vertex attributes -----
  vertex_ids <- as.numeric(V(g)$name)
  
  V(g)$age    <- hh$age[match(vertex_ids, hh$id)]
  V(g)$sex    <- hh$sex[match(vertex_ids, hh$id)]
  V(g)$is_hoh <- hh$is_hoh[match(vertex_ids, hh$id)]
  
  g
}


g <- household_to_graph(hh1)
g
components(g)$no  # Should return 1


g <- household_to_graph(hh2)
g
components(g)$no  # Should return 1

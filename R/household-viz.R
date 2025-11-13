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


library(igraph)

# ----- PEOPLE -----
# Woman is HOH (sex = 2), Man is spouse (sex = 1)
hh <- tibble::tibble(
  id         = c(1, 2, 3, 4, 5),
  age        = c(35, 37, 10, 7, 5),
  sex        = c(2, 1, 1, 2, 1),     # kids can be anything
  mother_id  = c(NA, NA, 1, 1, 1),   # woman is mother of all 3 kids
  father_id  = c(NA, NA, 2, 2, 2),   # man is father of all 3 kids
  spouse_id  = c(2, 1, NA, NA, NA),
  relate     = c("HOH", "Spouse", "Child", "Child", "Child")
)

# ----- EDGES (parent → child + spouse) -----
edges <- tibble::tibble(
  from = c(1, 2, 1, 2, 1, 2, 1, 2),   # parents
  to   = c(2, 1, 3, 3, 4, 4, 5, 5),   # spouse + kids
  type = c("spouse", "spouse",
           "parent", "parent",
           "parent", "parent",
           "parent", "parent")
)

# Remove duplicate spouse edges (1→2 and 2→1)
edges <- edges |> dplyr::distinct()

# ----- Build igraph -----
g <- igraph::graph_from_data_frame(
  edges,
  directed = TRUE,
  vertices = hh |> dplyr::rename(name = id)
)

# ----- Colors -----
sex_col <- ifelse(V(g)$sex == 2, "tomato", "steelblue")

# Thick outline on HOH
vertex_frame <- ifelse(V(g)$relate == "HOH", "black", "grey20")
vertex_lwd   <- ifelse(V(g)$relate == "HOH", 4, 1)

# ----- Manual layout -----
# Woman & man side-by-side at top
# Three kids below
layout_mat <- matrix(
  c( -1,  1,    # woman HOH
     1,  1,    # man
     -1, -1,    # child 1
     0, -1,    # child 2
     1, -1 ),  # child 3
  ncol = 2,
  byrow = TRUE
)

# ----- Plot -----
plot(
  g,
  layout = layout_mat,
  vertex.color = sex_col,
  vertex.label = V(g)$age,
  vertex.size = 35,
  vertex.frame.color = vertex_frame,
  vertex.label.color = "white",
  vertex.label.cex = 1.3,
  vertex.frame.width = vertex_lwd,
  edge.arrow.size = 0.7,
  edge.color = ifelse(E(g)$type == "spouse", "darkred", "grey40"),
  edge.lty   = ifelse(E(g)$type == "spouse", 1, 1)
)

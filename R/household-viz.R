# household-viz.R
# The purpose of this file is to define functions related to producing and visualizing 
# household-level network graphs

# Convert a household level table to a network graph
# the tables will look like what is in tests/test-data/household-fixtures/*.csv
library(readr)
library(igraph)
library(tibble)

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


household_to_graph <- function(hh) {
  edges <- list()
  
  # Parent-child edges
  for (i in seq_len(nrow(hh))) {
    if (!is.na(hh$mother_id[i])) {
      edges[[length(edges) + 1]] <- c(hh$mother_id[i], hh$id[i])
    }
    if (!is.na(hh$father_id[i])) {
      edges[[length(edges) + 1]] <- c(hh$father_id[i], hh$id[i])
    }
  }
  
  # Spouse edges
  for (i in seq_len(nrow(hh))) {
    if (!is.na(hh$spouse_id[i]) && hh$id[i] < hh$spouse_id[i]) {
      edges[[length(edges) + 1]] <- c(hh$id[i], hh$spouse_id[i])
    }
  }
  
  # Convert to matrix or data frame
  if (length(edges) == 0) {
    # No edges - create empty graph with all people as isolated nodes
    g <- igraph::make_empty_graph(n = nrow(hh), directed = FALSE)
    V(g)$name <- as.character(hh$id)
  } else {
    edge_df <- data.frame(
      from = sapply(edges, `[`, 1),
      to = sapply(edges, `[`, 2)
    )
    
    # Build graph with vertices data frame to ensure all people are included
    g <- igraph::graph_from_data_frame(
      d = edge_df,
      directed = FALSE,
      vertices = data.frame(name = hh$id)  # Ensure all people are vertices
    )
  }
  
  # Add vertex attributes
  vertex_ids <- as.numeric(V(g)$name)
  V(g)$age <- hh$age[match(vertex_ids, hh$id)]
  V(g)$sex <- hh$sex[match(vertex_ids, hh$id)]
  V(g)$is_hoh <- hh$is_hoh[match(vertex_ids, hh$id)]
  
  g
}

g <- household_to_graph(hh1)
g
components(g)$no  # Should return 1


g <- household_to_graph(hh2)
g
components(g)$no  # Should return 1

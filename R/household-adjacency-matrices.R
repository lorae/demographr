# The adjacency matrix in this function points from row -> col
# e.g. 
#    1   2
# 1  0   1
# 2  0   0
# means that "1 is the mother to 2"
parent_adjacency <- function(hh_tibble, parent_col, max_age = Inf) {
  # Initialize an adjacency matrix without edges
  m <- nrow(hh_tibble) 
  mat <- matrix(0, nrow = m, ncol = m) 
  
  # Directed (mother -> child) edges
  # Robust to both NA and 0 entries for non-relationships
  parent_edges <- hh_tibble |>
    filter(
      !is.na(.data[[parent_col]]), # NA
      .data[[parent_col]] != 0, # 0
      age <= max_age # Inf by default
    ) |>
    select(parent_id = !!sym(parent_col), id)
  
  # Add the parent edges
  mat[cbind(parent_edges$parent_id, parent_edges$id)] <- 1
  
  mat
}

# The adjacency matrix in this function is bidirectional due to the mutual
# nature of a spousal relationship
# e.g. 
#    1   2
# 1  0   1
# 2  1   0
# means "1 and 2 are spouses"
spouse_adjacency <- function(hh_tibble, spouse_col = "spouse_id") {
  # Initialize an adjacency matrix without edges
  m <- nrow(hh_tibble)
  mat <- matrix(0, m, m)
  
  spouse_edges <- hh_tibble |>
    filter(
      !is.na(.data[[spouse_col]]),
      .data[[spouse_col]] != 0
    ) |>
    select(id, spouse_id = all_of(spouse_col))
  
  if (nrow(spouse_edges) > 0) {
    # id → spouse
    mat[cbind(spouse_edges$id, spouse_edges$spouse_id)] <- 1
    # spouse → id (bidirectional)
    mat[cbind(spouse_edges$spouse_id, spouse_edges$id)] <- 1
  }
  
  mat
}

# Create a generic household adjacency matrix by summing above matrices
household_adjacency <- function(hh_tibble, max_age = Inf) {
  mat <- 
    parent_adjacency(hh_tibble, parent_col = "mother_id", max_age = max_age) +
    parent_adjacency(hh_tibble, parent_col = "father_id", max_age = max_age) +
    spouse_adjacency(hh_tibble)
  
  # Warn if household has self-edges
  if (any(diag(mat) != 0)) {
    warning("Adjacency matrix has at least one non-zero diagonal entry (an impossible self-edge of a parent-child or spouse relationship).")
  }
  
  # 2. Warn if any entry is not 0 or 1
  if (any(!(mat %in% c(0, 1)))) {
    warning("Adjacency matrix contains values other than 0 or 1 (an implausible self-edge of multiple relationship types for same two individuals).")
  }
  
  mat
}

# Count number of connected components in a household adjacency matrix
count_components <- function(mat) {
  # Convert directed adjacency to undirected
  undirected <- (mat + t(mat)) > 0

  # Build graph
  g <- igraph::graph_from_adjacency_matrix(undirected, mode = "undirected")
  
  # Number of connected components
  return(igraph::components(g))
}


#' Filter the graph of wave properties by path length and edge types
#'
#' @param path_length An integer specifying the number of edges (hops) between nodes.
#' @param relationships A vector of relationships to filter edges by. If NULL, all relationships are considered.
#' @return A data frame with pairs of nodes separated by the specified path length.
#' @export
filter_graph_by <- function(path_length, relationships = NULL) {
  graph = PROPERTY_RELATIONSHIPS
  # Validate inputs
  stopifnot(
    inherits(graph, "igraph"),
    is.numeric(path_length) && path_length == round(path_length)
  )

  # Return empty data frame with the usual columns if path_length is less than 0 or greater than 4
  if (path_length < 0 || path_length > 3) {
    return(data.frame(from = character(0), to = character(0), path_length = integer(0)))
  }

  # Optionally filter edges by relationships
  subgraph <- if (!is.null(relationships)) {
    edge_indices <- igraph::E(graph)[igraph::E(graph)$relationship %in% relationships]
    igraph::subgraph_from_edges(graph, edge_indices)
  } else {
    graph
  }

  # Calculate pairwise distances
  distances <- igraph::distances(subgraph, mode = "all")

  # Extract node pairs at the given path length
  valid_pairs <- which(distances == path_length, arr.ind = TRUE)

  # Return result as a data frame
  data.frame(
    from = igraph::V(subgraph)$name[valid_pairs[, 1]],
    to = igraph::V(subgraph)$name[valid_pairs[, 2]],
    path_length = path_length,
    stringsAsFactors = FALSE
  )
}


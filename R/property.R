# Define the generic property constructor
#' @export
property <- function(x, metadata) {
  UseMethod("property")
}

# Internal function for creating a property object
.property <- function(x, metadata = list()) {
  structure(
    modifyList(metadata, list(
      value = x
    )),
    class = 'property'
  )
}

# Method for creating a property from a numeric value
#' @export
property.numeric <- function(x, metadata = list()) {
  if (length(x) != 1) {
    stop("`x` must be of length 1")
  }
  .property(x, metadata)
}

# Generic conversion method for properties
#' @export
property.property <- function(x, metadata = list()) {
  if (length(x$value) != 1) {
    stop("`x` must be of length 1")
  }

  if (length(metadata) == 0 || class(x)[1] == metadata$class_name) {
    return(x)
  }

  converted = convert_from_to(x$value, x$class_name, metadata$class_name)

  .property(converted$value, metadata = metadata)
}

#' @export
property.default <- function(x) {
  stop("`x` must be numeric or a property object")
}

#' Convert value from one wave property to another via the shortest path
#' in the graph of all property types.
#'
#' @export
convert_from_to <- function(x, start_node, end_node) {

  # Check if the graph is available
  graph <- PROPERTY_RELATIONSHIPS

  # Use igraph to find the shortest path between the start and end nodes
  path <- igraph::shortest_paths(graph, from = start_node, to = end_node, output = "epath")

  # Check if a path exists
  if (length(path$epath[[1]]) == 0) {
    stop("No path found between nodes.")
  }

  new_x = x

  # Loop through the path and apply the corresponding transformation for each edge
  for (i in 1:(length(path$epath[[1]]))) {
    edge_id <- path$epath[[1]][i]
    func = PROPERTY_EDGES[edge_id,]$function_definition[[1]]
    new_x = func(new_x)
  }

  # Return the final transformed value
  return(list(
    value = new_x,
    edge_path = path$epath[[1]],
    edge_path_length = path$epath[[1]] %>% length(),
    property_edge = PROPERTY_EDGES[path$epath[[1]],]
  ))
}

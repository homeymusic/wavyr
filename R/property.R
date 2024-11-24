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

  value <- x$value

  # Apply rate_extent transformation (if needed)
  if (x$rate_extent != metadata$rate_extent) {
    value = 1 / value
  }

  # Apply space_time transformation (if needed)
  if (x$space_time != metadata$space_time) {
    if (x$rate_extent == EXTENT_RATE$rate) {
      if (x$space_time == TIME_SPACE$space && metadata$space_time == TIME_SPACE$time) {
        value = value * DEFAULT_SPEED_OF_MEDIUM
      } else if (x$space_time == TIME_SPACE$time && metadata$space_time == TIME_SPACE$space) {
        value = value / DEFAULT_SPEED_OF_MEDIUM
      }
    } else {
      if (x$space_time == TIME_SPACE$space && metadata$space_time == TIME_SPACE$time) {
        value = value / DEFAULT_SPEED_OF_MEDIUM
      } else if (x$space_time == TIME_SPACE$time && metadata$space_time == TIME_SPACE$space) {
        value = value * DEFAULT_SPEED_OF_MEDIUM
      }
    }
  }

  # Apply linear_angular transformation (if needed)
  if (x$linear_angular != metadata$linear_angular) {
    if (x$rate_extent == EXTENT_RATE$rate) {
      if (x$linear_angular == LINEAR_ANGULAR$linear && metadata$linear_angular == LINEAR_ANGULAR$angular) {
        value = 2 * pi * value
      } else if (x$linear_angular == LINEAR_ANGULAR$angular && metadata$linear_angular == LINEAR_ANGULAR$linear) {
        value = value / (2 * pi)
      }
    } else {
      if (x$linear_angular == LINEAR_ANGULAR$linear && metadata$linear_angular == LINEAR_ANGULAR$angular) {
        value = value / (2 * pi)
      } else if (x$linear_angular == LINEAR_ANGULAR$angular && metadata$linear_angular == LINEAR_ANGULAR$linear) {
        value = 2 * pi * value
      }
    }
  }
  # Construct the new property
  .property(value, metadata = metadata)
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
    function_definition = PROPERTY_EDGES[edge_id,]$function_definition[[1]]
    new_x = do.call(function_definition, list(new_x))
  }

  # Return the final transformed value
  return(list(
    value = new_x,
    function_label = PROPERTY_EDGES[path$epath[[1]],]$function_label,
    edge_path = path$epath[[1]],
    edge_path_length = path$epath[[1]] %>% length()
  ))
}

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
    if (x$rate_extent == RATE_EXTENT$rate) {
      if (x$space_time == SPACE_TIME$space && metadata$space_time == SPACE_TIME$time) {
        value = value * DEFAULT_SPEED_OF_MEDIUM
      } else if (x$space_time == SPACE_TIME$time && metadata$space_time == SPACE_TIME$space) {
        value = value / DEFAULT_SPEED_OF_MEDIUM
      }
    } else {
      if (x$space_time == SPACE_TIME$space && metadata$space_time == SPACE_TIME$time) {
        value = value / DEFAULT_SPEED_OF_MEDIUM
      } else if (x$space_time == SPACE_TIME$time && metadata$space_time == SPACE_TIME$space) {
        value = value * DEFAULT_SPEED_OF_MEDIUM
      }
    }
  }

  # Apply linear_angular transformation (if needed)
  if (x$linear_angular != metadata$linear_angular) {
    if (x$rate_extent == RATE_EXTENT$rate) {
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

  # Loop through the path and apply the corresponding transformation for each edge
  for (i in 1:(length(path$epath[[1]]))) {
    edge_id <- path$epath[[1]][i]

    # Get the relationship label and mathematical relationship for the current edge
    edge <- PROPERTY_EDGES[PROPERTY_EDGES$edge_id == edge_id, ]

    # Apply the transformation specified for this edge (mathematical relationship)
    # This can be expanded based on the edge's mathematical function
    current_value <- apply_transformation(current_value, edge$mathematical_relationship)
  }

  # Return the final transformed value
  return(current_value)
}

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

#' @export
SPACE_TIME     <- list(space = "space", time = "time",
                       label = "Space ~ Time",
                       expression = "Space \u2194 Time")
#' @export
LINEAR_ANGULAR <- list(linear = "linear", angular = "angular",
                       label = "Linear ~ Angular",
                       expression = "Linear \u2194 Angular")
#' @export
RATE_EXTENT    <- list(extent = "extent", rate = "rate",
                       label = "Rate ~ Extent",
                       expression = "Rate \u2194 Extent")

PROPERTIES <- list(
  angular_frequency = 'angular_frequency',
  angular_period = 'angular_period',
  angular_wavelength = 'angular_wavelength',
  angular_wavenumber = 'angular_wavenumber',
  linear_frequency = 'linear_frequency',
  linear_period = 'linear_period',
  linear_wavelength = 'linear_wavelength',
  linear_wavenumber = 'linear_wavenumber'
)

PROPERTY_NODES <- data.frame(
  name = c(
    PROPERTIES$linear_frequency, PROPERTIES$linear_period, PROPERTIES$linear_wavenumber, PROPERTIES$linear_wavelength,
    PROPERTIES$angular_frequency, PROPERTIES$angular_period, PROPERTIES$angular_wavenumber, PROPERTIES$angular_wavelength
  ),
  label = c(
    'atop(italic(f), atop("Linear Frequency", "(linear time rate)"))',
    'atop(italic(T), atop("Linear Period", "(linear time extent)"))',
    'atop(italic(k)["linear"], atop("Linear Wavenumber", "(linear space rate)"))',
    'atop(italic(λ), atop("Linear Wavelength", "(linear space extent)"))',
    'atop(italic(ω), atop("Angular Frequency", "(angular time rate)"))',
    'atop(italic(τ)["angular"], atop("Angular Period", "(angular time extent)"))',
    'atop(italic(k), atop("Angular Wavenumber", "(angular space rate)"))',
    'atop(italic("\u019B")["angular"], atop("Angular Wavelength", "(angular space extent)"))'  # Lambda with a slash
  ),
  x = c(0, 0, 1, 1, 2, 2, 3, 3),  # Adjusted x-coordinates for a cube structure
  y = c(2, 0, 3, 1, 2, 0, 3, 1)   # Adjusted y-coordinates for a cube structure
)

PROPERTY_EDGES <- data.frame(
  from = c(
    PROPERTIES$linear_frequency, PROPERTIES$linear_wavenumber, PROPERTIES$angular_frequency, PROPERTIES$angular_wavenumber,
    PROPERTIES$linear_frequency, PROPERTIES$linear_wavenumber, PROPERTIES$linear_period, PROPERTIES$linear_wavelength,
    PROPERTIES$linear_frequency, PROPERTIES$linear_period, PROPERTIES$angular_frequency, PROPERTIES$angular_period
  ),
  to = c(
    PROPERTIES$linear_period, PROPERTIES$linear_wavelength, PROPERTIES$angular_period, PROPERTIES$angular_wavelength,
    PROPERTIES$angular_frequency, PROPERTIES$angular_wavenumber, PROPERTIES$angular_period, PROPERTIES$angular_wavelength,
    PROPERTIES$linear_wavenumber, PROPERTIES$linear_wavelength, PROPERTIES$angular_wavenumber, PROPERTIES$angular_wavelength
  ),
  relationship = c(
    rep(RATE_EXTENT$label, 4),
    rep(LINEAR_ANGULAR$label, 4),
    rep(SPACE_TIME$label, 4)
  ),
  mathematical_relationship = c(
    # Rate ~ Extent
    "1 / x", "1 / x", "1 / x", "1 / x",
    # Linear ~ Angular
    "2 * pi %.% x", "2 * pi %.% x", "x / (2 * pi)", "x / (2 * pi)",
    # Time ~ Space
    "x %.% c", "x / c", "x %.% c", "x / c"
  )
)


# Create the graph as an undirected graph
PROPERTY_RELATIONSHIPS <- igraph::graph_from_data_frame(
  d = PROPERTY_EDGES,
  vertices = PROPERTY_NODES,
  directed = FALSE
)

PROPERTY_RELATIONSHIPS_PLOT <- ggraph::ggraph(PROPERTY_RELATIONSHIPS, layout = "manual", x = PROPERTY_NODES$x, y = PROPERTY_NODES$y) +
  # Use arcs for edges with subtle radii
  ggraph::geom_edge_arc(
    ggplot2::aes(label = relationship),
    angle_calc = 'along',
    arrow = NULL, # Remove the arrowheads for undirected graph
    end_cap = ggraph::circle(3, 'mm'),
    edge_width = 0.8,
    color = "gray", # Set arcs to gray
    strength = 0.0,
    label_parse = T,
    label_size = 3,
    vjust = -0.8, # Adjust placement for visibility
    label_colour = "gray"  # Set labels to gray
  ) +
  ggraph::geom_edge_arc(
    ggplot2::aes(label = mathematical_relationship),
    angle_calc = 'along',
    arrow = NULL, # Remove the arrowheads for undirected graph
    end_cap = ggraph::circle(3, 'mm'),
    edge_width = 0.8,
    color = "gray", # Set arcs to gray
    strength = 0.0,
    label_parse = T,
    label_size = 3,
    vjust = 1.5, # Adjust placement for visibility
    label_colour = "gray"  # Set labels to gray
  ) +
  # Add PROPERTY_NODES with light blue color
  ggraph::geom_node_point(size = 8, color = "darkgray") +
  # Add English title case node labels
  ggraph::geom_node_text(
    ggplot2::aes(label = label),
    parse = T,
    nudge_y = 0.3, # Offset node labels slightly
    size = 4
  ) +
  # Add title and expand the plot space
  ggplot2::ggtitle("Wave Properties") +
  ggraph::theme_graph(base_family = "sans") +
  ggplot2::theme(plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.2)) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.2)) +
  ggplot2::coord_fixed(ratio = 1)  # Ensure equal aspect ratio for x and y axes


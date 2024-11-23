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

  # Debugging output
  print(paste("Converting from", class(x)[1], "to", metadata$class_name))
  print(paste("Initial value:", value))

  # Apply measure transformation (if needed)
  if (x$measure != metadata$measure) {
    value = 1 / value
    print(paste("After measure adjustment:", value))
  }

  # Apply dimension transformation (if needed)
  if (x$dimension != metadata$dimension) {
    value = value * DEFAULT_SPEED_OF_MEDIUM
    print(paste("After dimension adjustment:", value))
  }

  # Apply rotation transformation (if needed)
  if (x$rotation != metadata$rotation) {
    value = 2 * pi * value
    print(paste("After rotation adjustment:", value))
  }

  # Debugging output
  print(paste("Final value:", value))

  # Construct the new property
  .property(value, metadata = metadata)
}

#' @export
property.default <- function(x) {
  stop("`x` must be numeric or a property object")
}

#' @export
Dimension <- list(spatial = "spatial", temporal = "temporal")
#' @export
Rotation  <- list(linear = "linear", angular = "angular")
#' @export
Measure   <- list(extent = "extent", rate = "rate")

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

# Define nodes with 3D-like positions (for a cube)
PROPERTY_NODES <- data.frame(
  name = c(
    PROPERTIES$linear_frequency, PROPERTIES$linear_period, PROPERTIES$linear_wavenumber, PROPERTIES$linear_wavelength,
    PROPERTIES$angular_frequency, PROPERTIES$angular_period, PROPERTIES$angular_wavenumber, PROPERTIES$angular_wavelength
  ),
  x = c(0, 0, 1, 1, 2, 2, 3, 3), # Adjusted x-coordinates for a cube structure
  y = c(2, 0, 3, 1, 2, 0, 3, 1)  # Adjusted y-coordinates for a cube structure
)

# Define 12 unique edges (undirected, no redundancy)
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
    "Measure", "Measure", "Measure", "Measure",
    "Rotation", "Rotation", "Rotation", "Rotation",
    "Dimension", "Dimension", "Dimension", "Dimension"
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
    # label_dodge = ggplot2::unit(2.5, 'mm'),
    arrow = NULL, # Remove the arrowheads for undirected graph
    end_cap = ggraph::circle(3, 'mm'),
    edge_width = 0.8,
    color = "gray", # Set arcs to gray
    strength = 0.0,
    label_size = 3,
    vjust = -0.3,
    label_colour = "gray"  # Set labels to gray
  ) +
  # Add PROPERTY_NODES with light blue color
  ggraph::geom_node_point(size = 8, color = "lightblue") +
  # Add node labels
  ggraph::geom_node_text(
    ggplot2::aes(label = name),
    nudge_y = 0.15, # Offset node labels slightly
    size = 4
  ) +
  # Add title and expand the plot space
  ggplot2::ggtitle("Wave Properties") +
  ggraph::theme_graph(base_family = "sans") +
  ggplot2::theme(plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.2)) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.2))

#' Get node pairs that are a specific path length apart in an igraph object
#'
#' @param path_length An integer specifying the number of edges (hops) between nodes.
#' @param relationships A vector of relationships to filter edges by. If NULL, all relationships are considered.
#' @return A data frame with pairs of nodes separated by the specified path length.
#' @export
property_relationships <- function(path_length, relationships = NULL) {
  graph = PROPERTY_RELATIONSHIPS
  # Validate inputs
  stopifnot(
    inherits(graph, "igraph"),
    is.numeric(path_length) && path_length >= 0 && path_length == round(path_length)
  )

  # Optionally filter edges by relationships
  subgraph <- if (!is.null(relationships)) {
    edge_indices <- igraph::E(graph)[igraph::E(graph)$relationship %in% relationships]
    igraph::subgraph.edges(graph, edge_indices)
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

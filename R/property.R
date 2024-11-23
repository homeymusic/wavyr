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
property.numeric <- function(x, metadata=list()) {
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
    value = 2  * pi * value
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
  angular_frequency='angular_frequency',
  angular_period='angular_period',
  angular_wavelength='angular_wavelength',
  angular_wavenumber='angular_wavenumber',
  linear_frequency='linear_frequency',
  linear_period='linear_period',
  linear_wavelength='linear_wavelength',
  linear_wavenumber='linear_wavenumber'
)

# Define nodes with 3D-like positions (for a cube)
PROPERTY_NODES <- tibble::tibble(
  name = c(
    PROPERTIES$linear_frequency, PROPERTIES$linear_period, PROPERTIES$linear_wavenumber, PROPERTIES$linear_wavelength,
    PROPERTIES$angular_frequency, PROPERTIES$angular_period, PROPERTIES$angular_wavenumber, PROPERTIES$angular_wavelength
  ),
  x = c(0, 0, 1, 1, 2, 2, 3, 3), # Adjusted x-coordinates for a cube structure
  y = c(2, 0, 3, 1, 2, 0, 3, 1)  # Adjusted y-coordinates for a cube structure
)

# Define 12 unique edges (undirected, no redundancy)
PROPERTY_EDGES <- tibble::tibble(
  from = c(
    PROPERTIES$linear_frequency, PROPERTIES$linear_wavenumber,PROPERTIES$angular_frequency, PROPERTIES$angular_wavenumber,
    PROPERTIES$linear_frequency, PROPERTIES$linear_wavenumber,PROPERTIES$linear_period, PROPERTIES$linear_wavelength,
    PROPERTIES$linear_frequency, PROPERTIES$linear_period, PROPERTIES$angular_frequency, PROPERTIES$angular_period
  ),
  to = c(
    PROPERTIES$linear_period, PROPERTIES$linear_wavelength,PROPERTIES$angular_period, PROPERTIES$angular_wavelength,
    PROPERTIES$angular_frequency, PROPERTIES$angular_wavenumber,PROPERTIES$angular_period, PROPERTIES$angular_wavelength,
    PROPERTIES$linear_wavenumber, PROPERTIES$linear_wavelength, PROPERTIES$angular_wavenumber, PROPERTIES$angular_wavelength
  ),
  relationship = c(
    "Measure", "Measure", "Measure", "Measure",
    "Rotation", "Rotation", "Rotation", "Rotation",
    "Dimension", "Dimension", "Dimension", "Dimension"
  )
)

# Create the graph as an undirected graph (with no redundant edges)
PROPERTY_RELATIONSHIPS <- tidygraph::tbl_graph(nodes = PROPERTY_NODES, edges = PROPERTY_EDGES, directed = FALSE)

# Visualize the graph with gray arcs and light blue PROPERTY_NODES
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

#' Get node pairs that are a certain number of degrees apart in a tidygraph object
#'
#' @param degree An integer indicating the number of degrees (edges) between nodes
#' @param relationships A vector of relationships, optional. If provided, it must match the degree length.
#' @return A data frame with pairs of nodes that are 'degree' edges apart
#' @export
property_relationships <- function(degree, relationships = NULL) {

  # Check if 'degree' is valid (should be non-negative integer)
  if (!is.numeric(degree) || degree < 0 || degree != round(degree)) {
    stop("`degree` should be a non-negative integer.")
  }

  # If relationships are provided, check if the length matches the degree
  if (!is.null(relationships)) {
    if (length(relationships) != degree) {
      stop("The length of relationships must match the degree.")
    }
  }

  # If degree is 0, return a list of all nodes paired with themselves
  if (degree == 0) {
    node_names <- PROPERTY_RELATIONSHIPS %>%
      tidygraph::activate(nodes) %>%
      dplyr::pull(name)  # Extract the node names

    # Return a tibble with each node paired with itself
    return(tibble::tibble(from = node_names, to = node_names))
  }

  # If relationships are provided, filter edges by relationships
  edges <- PROPERTY_EDGES
  if (!is.null(relationships)) {
    edges <- edges %>%
      dplyr::filter(relationship %in% relationships)
  }

  # Calculate all pairwise distances using igraph::distances
  distances_matrix <- PROPERTY_RELATIONSHIPS %>%
    tidygraph::activate(nodes) %>%
    igraph::distances()  # Use igraph::distances() for shortest paths

  # Convert the distances matrix to a data frame
  distances_df <- as.data.frame(distances_matrix)

  # Add row and column names for 'from' and 'to' nodes
  distances_df$rowid <- rownames(distances_df)
  distances_df <- tidyr::pivot_longer(distances_df,
                                      cols = -rowid,
                                      names_to = "to",
                                      values_to = "distance")

  # Filter for pairs of nodes that are exactly 'degree' apart
  result_df <- distances_df %>%
    dplyr::filter(distance == degree) %>%
    dplyr::select(from = rowid, to, distance)  # Rename and select relevant columns

  # Add the relationships column if provided (or leave it blank if relationships is NULL)
  if (!is.null(relationships)) {
    result_df$relationships <- relationships
  }

  # Filter by relationships only if relationships is not NULL
  if (!is.null(relationships)) {
    result_df <- result_df %>%
      dplyr::filter(paste(from, to) %in% paste(edges$from, edges$to))
  }

  # Return the result
  return(result_df)
}

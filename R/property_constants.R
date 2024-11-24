# Define the dimensions
LINEAR_ANGULAR <- list(linear = "linear", angular = "angular")
SPACE_TIME     <- list(space = "space", time = "time")
RATE_EXTENT    <- list(extent = "extent", rate = "rate")

DIMENSIONS <- list(
  linear_angular = LINEAR_ANGULAR,
  space_time = SPACE_TIME,
  rate_extent = RATE_EXTENT
)

NODES <- expand.grid(
  LINEAR_ANGULAR = unlist(LINEAR_ANGULAR),
  SPACE_TIME = unlist(SPACE_TIME),
  RATE_EXTENT = unlist(RATE_EXTENT)
)

NODES$description <- apply(NODES, 1, function(row) paste(row, collapse = ", "))

# Function to find rows matching exactly 2 of 3 columns
find_matches <- function(target_row, nodes) {
  matches <- apply(nodes, 1, function(row) sum(row == target_row))
  nodes[matches == 2, , drop = FALSE]
}

# Loop through all rows and collect results
EDGE_IDS <- do.call(rbind, lapply(1:nrow(NODES), function(i) {
  matches <- find_matches(NODES[i, ], NODES)
  # Add "from" and "to" columns to track edges
  data.frame(from = i, to = which(apply(NODES, 1, function(row) sum(row == NODES[i, ])) == 2))
}))

# Create the EDGES table using EDGE_IDS
EDGES <- data.frame(
  from = NODES$description[EDGE_IDS$from],
  to = NODES$description[EDGE_IDS$to]
)

ANGULAR_FREQUENCY <- data.frame(
  name = "angular frequency",
  class_name = "angular_frequency",
  unit = "rad/s",
  unit_latex = "\\frac{\\text{rad}}{\\text{s}}",
  symbol = "\u03C9",
  symbol_latex = "\\omega",
  symbol_expression = "italic(ω)",
  space_time = SPACE_TIME$time,
  linear_angular = LINEAR_ANGULAR$angular,
  rate_extent = RATE_EXTENT$rate
)

ANGULAR_PERIOD <- data.frame(
  name = 'angular period',
  class_name = 'angular_period',
  unit = "s/rad",
  unit_latex = "\\frac{\\text{s}}{\\text{rad}}",
  symbol = "T_angular",
  symbol_latex = "\\Tau_\\text{angular}",
  symbol_expression = 'italic(τ)["angular"]',
  space_time = SPACE_TIME$time,
  linear_angular = LINEAR_ANGULAR$angular,
  rate_extent = RATE_EXTENT$extent
)

ANGULAR_WAVELENGTH <-data.frame(
  name         = "angular wavelength",
  class_name   = "angular_wavelength",
  unit         = "m/rad",
  unit_latex   = "\\frac{\\text{m}}{\\text{rad}}",
  symbol       = "l_angular",
  symbol_latex = "\\lambda_\\text{angular}",
  symbol_expression = "italic(λ)",
  linear_angular    = LINEAR_ANGULAR$angular,
  space_time        = SPACE_TIME$space,
  rate_extent       = RATE_EXTENT$extent
)

ANGULAR_WAVENUMBER <-data.frame(
  name = "angular wavenumber",
  class_name = "angular_wavenumber",
  space_time = SPACE_TIME$space,
  linear_angular = LINEAR_ANGULAR$angular,
  rate_extent = RATE_EXTENT$rate,
  unit = "rad/m",
  unit_latex = "\\frac{\\text{rad}}{\\text{m}}",
  symbol = "k_angular",
  symbol_latex = "k_\\text{angular}",
  symbol_expression = "italic(k)"
)

LINEAR_FREQUENCY <- data.frame(
  name = "linear frequency",
  class_name = 'linear_frequency',
  unit = "Hz",
  unit_latex = "\\text{Hz}",
  symbol = "f",
  symbol_latex = "f",
  symbol_expression = "italic(f)",
  space_time = SPACE_TIME$time,
  linear_angular = LINEAR_ANGULAR$linear,
  rate_extent = RATE_EXTENT$rate
)

LINEAR_PERIOD <- data.frame(
  name = "linear period",
  class_name = 'linear_period',
  unit = "s",
  unit_latex = "\\text{s}",
  symbol = "T",
  symbol_latex = "T",
  symbol_expression = "italic(T)",
  space_time = SPACE_TIME$time,
  linear_angular = LINEAR_ANGULAR$linear,
  rate_extent = RATE_EXTENT$extent
)

LINEAR_WAVELENGTH <- data.frame(
  class_name = 'linear_wavelength',
  space_time = SPACE_TIME$space,
  linear_angular = LINEAR_ANGULAR$linear,
  rate_extent = RATE_EXTENT$extent,
  unit = "m",
  unit_latex = "\\text{m}",
  symbol = "\u03BB",
  symbol_latex = "\\lambda",
  symbol_expression = "italic(λ)",
  name = "linear wavelength"
)

LINEAR_WAVENUMBER <- data.frame(
  class_name = 'linear_wavenumber',
  space_time = SPACE_TIME$space,
  linear_angular = LINEAR_ANGULAR$linear,
  rate_extent = RATE_EXTENT$rate,
  unit = "1/m",
  unit_latex = "\\text{m}^{-1}",
  symbol = "k_linear",
  symbol_latex = "k_{\\text{linear}}",
  symbol_expression = 'italic(k)["linear"]',
  name = "linear wavenumber"
)

PROPERTIES <- rbind(
  linear_frequency   = LINEAR_FREQUENCY,
  linear_period      = LINEAR_PERIOD,
  linear_wavenumber  = LINEAR_WAVENUMBER,
  linear_wavelength  = LINEAR_WAVELENGTH,
  angular_frequency  = ANGULAR_FREQUENCY,
  angular_period     = ANGULAR_PERIOD,
  angular_wavenumber = ANGULAR_WAVENUMBER,
  angular_wavelength = ANGULAR_WAVELENGTH
)

# Function to create the label dynamically
assemble_label <- function(property) {
  paste0(
    'atop(',
    property$symbol_expression, ', ',
    'atop("', property$name, '", ',
    '"[', property$linear_angular, ' ', property$space_time, ' ', property$rate_extent, ']"))'
  )
}

PROPERTY_NODES <- data.frame(
  name = sapply(PROPERTIES, function(property) property$class_name),
  label = sapply(PROPERTIES, assemble_label),
  x = c(0, 0, 1, 1, 2, 2, 3, 3),  # Adjusted x-coordinates for a cube structure
  y = c(2, 0, 3, 1, 2, 0, 3, 1)   # Adjusted y-coordinates for a cube structure
)

PROPERTY_EDGES <- data.frame(
  from = c(
    PROPERTIES$linear_frequency$class_name, PROPERTIES$linear_wavenumber$class_name, PROPERTIES$angular_frequency$class_name, PROPERTIES$angular_wavenumber$class_name,
    PROPERTIES$linear_frequency$class_name, PROPERTIES$linear_wavenumber$class_name, PROPERTIES$linear_period$class_name, PROPERTIES$linear_wavelength$class_name,
    PROPERTIES$linear_frequency$class_name, PROPERTIES$linear_period$class_name, PROPERTIES$angular_frequency$class_name, PROPERTIES$angular_period$class_name
  ),
  to = c(
    PROPERTIES$linear_period$class_name, PROPERTIES$linear_wavelength$class_name, PROPERTIES$angular_period$class_name, PROPERTIES$angular_wavelength$class_name,
    PROPERTIES$angular_frequency$class_name, PROPERTIES$angular_wavenumber$class_name, PROPERTIES$angular_period$class_name, PROPERTIES$angular_wavelength$class_name,
    PROPERTIES$linear_wavenumber$class_name, PROPERTIES$linear_wavelength$class_name, PROPERTIES$angular_wavenumber$class_name, PROPERTIES$angular_wavelength$class_name
  )
)

FN_DOUBLE_X <- function(x) {
  2 * x
}
PROPERTY_EDGES$function_definition <- rep(list(FN_DOUBLE_X), times = 12)

# Create the graph as an undirected graph
PROPERTY_RELATIONSHIPS <- igraph::graph_from_data_frame(
  d = PROPERTY_EDGES,
  vertices = PROPERTY_NODES,
  directed = FALSE
)

PROPERTY_RELATIONSHIPS_PLOT <- ggraph::ggraph(PROPERTY_RELATIONSHIPS, layout = "manual", x = PROPERTY_NODES$x, y = PROPERTY_NODES$y) +
  # Use arcs for edges with subtle radii
  ggraph::geom_edge_arc(
    ggplot2::aes(label = relationship_expression),
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
    ggplot2::aes(label = function_label),
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


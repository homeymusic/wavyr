SPACE_TIME     <- list(space = "space", time = "time",
                       label = "space ~ time",
                       expression = "space %<->% time")
LINEAR_ANGULAR <- list(linear = "linear", angular = "angular",
                       label = "linear ~ angular",
                       expression = "linear %<->% angular")
RATE_EXTENT    <- list(extent = "extent", rate = "rate",
                       label = "rate ~ extent",
                       expression = "rate %<->% extent")

FN_DOUBLE_X <- function(x) {
  2 * x
}

ANGULAR_FREQUENCY <- list(
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

ANGULAR_PERIOD <- list(
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

ANGULAR_WAVELENGTH <-list(
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

ANGULAR_WAVENUMBER <-list(
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

LINEAR_FREQUENCY <- list(
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

LINEAR_PERIOD <- list(
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

LINEAR_WAVELENGTH <- list(
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

LINEAR_WAVENUMBER <- list(
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

PROPERTIES <- list(
  angular_frequency  = ANGULAR_FREQUENCY,
  angular_period     = ANGULAR_PERIOD,
  angular_wavelength = ANGULAR_WAVELENGTH,
  angular_wavenumber = ANGULAR_WAVENUMBER,
  linear_frequency   = LINEAR_FREQUENCY,
  linear_period      = LINEAR_PERIOD,
  linear_wavelength  = LINEAR_WAVELENGTH,
  linear_wavenumber  = LINEAR_WAVENUMBER
)
#
# PROPERTY_NODES <- data.frame(
#   name = c(
#     PROPERTIES$linear_frequency, PROPERTIES$linear_period, PROPERTIES$linear_wavenumber, PROPERTIES$linear_wavelength,
#     PROPERTIES$angular_frequency, PROPERTIES$angular_period, PROPERTIES$angular_wavenumber, PROPERTIES$angular_wavelength
#   ),
#   label = c(
#     'atop(italic(f), atop("Linear Frequency", "(linear time rate)"))',
#     'atop(italic(T), atop("Linear Period", "(linear time extent)"))',
#     'atop(italic(k)["linear"], atop("Linear Wavenumber", "(linear space rate)"))',
#     'atop(italic(λ), atop("Linear Wavelength", "(linear space extent)"))',
#     'atop(italic(ω), atop("Angular Frequency", "(angular time rate)"))',
#     'atop(italic(τ)["angular"], atop("Angular Period", "(angular time extent)"))',
#     'atop(italic(k), atop("Angular Wavenumber", "(angular space rate)"))',
#     'atop(italic("\u019B")["angular"], atop("Angular Wavelength", "(angular space extent)"))'  # Lambda with a slash
#   ),
#   x = c(0, 0, 1, 1, 2, 2, 3, 3),  # Adjusted x-coordinates for a cube structure
#   y = c(2, 0, 3, 1, 2, 0, 3, 1)   # Adjusted y-coordinates for a cube structure
# )
#
# PROPERTY_EDGES <- data.frame(
#   from = c(
#     PROPERTIES$linear_frequency, PROPERTIES$linear_wavenumber, PROPERTIES$angular_frequency, PROPERTIES$angular_wavenumber,
#     PROPERTIES$linear_frequency, PROPERTIES$linear_wavenumber, PROPERTIES$linear_period, PROPERTIES$linear_wavelength,
#     PROPERTIES$linear_frequency, PROPERTIES$linear_period, PROPERTIES$angular_frequency, PROPERTIES$angular_period
#   ),
#   to = c(
#     PROPERTIES$linear_period, PROPERTIES$linear_wavelength, PROPERTIES$angular_period, PROPERTIES$angular_wavelength,
#     PROPERTIES$angular_frequency, PROPERTIES$angular_wavenumber, PROPERTIES$angular_period, PROPERTIES$angular_wavelength,
#     PROPERTIES$linear_wavenumber, PROPERTIES$linear_wavelength, PROPERTIES$angular_wavenumber, PROPERTIES$angular_wavelength
#   ),
#   relationship = c(
#     rep(RATE_EXTENT$label, 4),
#     rep(LINEAR_ANGULAR$label, 4),
#     rep(SPACE_TIME$label, 4)
#   ),
#   relationship_expression = c(
#     rep(RATE_EXTENT$expression, 4),
#     rep(LINEAR_ANGULAR$expression, 4),
#     rep(SPACE_TIME$expression, 4)
#   ),
#   function_label = c(
#     # Rate ~ Extent
#     "1 / x", "1 / x", "1 / x", "1 / x",
#     # Linear ~ Angular
#     "2 * pi %.% x", "2 * pi %.% x", "x / (2 * pi)", "x / (2 * pi)",
#     # Time ~ Space
#     "x %.% c", "x / c", "x %.% c", "x / c"
#   )
# )
#
# PROPERTY_EDGES$function_definition <- rep(list(FN_DOUBLE_X), times = 12)
#
# # Create the graph as an undirected graph
# PROPERTY_RELATIONSHIPS <- igraph::graph_from_data_frame(
#   d = PROPERTY_EDGES,
#   vertices = PROPERTY_NODES,
#   directed = FALSE
# )
#
# PROPERTY_RELATIONSHIPS_PLOT <- ggraph::ggraph(PROPERTY_RELATIONSHIPS, layout = "manual", x = PROPERTY_NODES$x, y = PROPERTY_NODES$y) +
#   # Use arcs for edges with subtle radii
#   ggraph::geom_edge_arc(
#     ggplot2::aes(label = relationship_expression),
#     angle_calc = 'along',
#     arrow = NULL, # Remove the arrowheads for undirected graph
#     end_cap = ggraph::circle(3, 'mm'),
#     edge_width = 0.8,
#     color = "gray", # Set arcs to gray
#     strength = 0.0,
#     label_parse = T,
#     label_size = 3,
#     vjust = -0.8, # Adjust placement for visibility
#     label_colour = "gray"  # Set labels to gray
#   ) +
#   ggraph::geom_edge_arc(
#     ggplot2::aes(label = function_label),
#     angle_calc = 'along',
#     arrow = NULL, # Remove the arrowheads for undirected graph
#     end_cap = ggraph::circle(3, 'mm'),
#     edge_width = 0.8,
#     color = "gray", # Set arcs to gray
#     strength = 0.0,
#     label_parse = T,
#     label_size = 3,
#     vjust = 1.5, # Adjust placement for visibility
#     label_colour = "gray"  # Set labels to gray
#   ) +
#   # Add PROPERTY_NODES with light blue color
#   ggraph::geom_node_point(size = 8, color = "darkgray") +
#   # Add English title case node labels
#   ggraph::geom_node_text(
#     ggplot2::aes(label = label),
#     parse = T,
#     nudge_y = 0.3, # Offset node labels slightly
#     size = 4
#   ) +
#   # Add title and expand the plot space
#   ggplot2::ggtitle("Wave Properties") +
#   ggraph::theme_graph(base_family = "sans") +
#   ggplot2::theme(plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
#   ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.2)) +
#   ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.2)) +
#   ggplot2::coord_fixed(ratio = 1)  # Ensure equal aspect ratio for x and y axes
#

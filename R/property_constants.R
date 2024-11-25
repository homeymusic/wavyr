LINEAR_ANGULAR <- list(linear = "linear", angular = "angular")
TIME_SPACE     <- list(space  = "space", time = "time")
EXTENT_RATE    <- list(extent = "extent", rate = "rate")

DIMENSIONS <- list(
  linear_angular = LINEAR_ANGULAR,
  space_time = TIME_SPACE,
  rate_extent = EXTENT_RATE
)

ANGULAR_FREQUENCY <- data.frame(
  name = "angular frequency",
  class_name = "angular_frequency",
  unit = "rad/s",
  unit_latex = "\\frac{\\text{rad}}{\\text{s}}",
  symbol = "\u03C9",
  symbol_latex = "\\omega",
  symbol_expression = "italic(ω)",
  space_time = TIME_SPACE$time,
  linear_angular = LINEAR_ANGULAR$angular,
  rate_extent = EXTENT_RATE$rate
)

ANGULAR_PERIOD <- data.frame(
  name = 'angular period',
  class_name = 'angular_period',
  unit = "s/rad",
  unit_latex = "\\frac{\\text{s}}{\\text{rad}}",
  symbol = "T_angular",
  symbol_latex = "\\Tau_\\text{angular}",
  symbol_expression = 'italic(T)["angular"]',
  space_time = TIME_SPACE$time,
  linear_angular = LINEAR_ANGULAR$angular,
  rate_extent = EXTENT_RATE$extent
)

ANGULAR_WAVELENGTH <-data.frame(
  name         = "angular wavelength",
  class_name   = "angular_wavelength",
  unit         = "m/rad",
  unit_latex   = "\\frac{\\text{m}}{\\text{rad}}",
  symbol       = "l_angular",
  symbol_latex = "\\lambda_\\text{angular}",
  symbol_expression = 'italic(ƛ)["angular"]',
  linear_angular    = LINEAR_ANGULAR$angular,
  space_time        = TIME_SPACE$space,
  rate_extent       = EXTENT_RATE$extent
)

ANGULAR_WAVENUMBER <-data.frame(
  name = "angular wavenumber",
  class_name = "angular_wavenumber",
  space_time = TIME_SPACE$space,
  linear_angular = LINEAR_ANGULAR$angular,
  rate_extent = EXTENT_RATE$rate,
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
  space_time = TIME_SPACE$time,
  linear_angular = LINEAR_ANGULAR$linear,
  rate_extent = EXTENT_RATE$rate
)

LINEAR_PERIOD <- data.frame(
  name = "linear period",
  class_name = 'linear_period',
  unit = "s",
  unit_latex = "\\text{s}",
  symbol = "T",
  symbol_latex = "T",
  symbol_expression = "italic(T)",
  space_time = TIME_SPACE$time,
  linear_angular = LINEAR_ANGULAR$linear,
  rate_extent = EXTENT_RATE$extent
)

LINEAR_WAVELENGTH <- data.frame(
  class_name = 'linear_wavelength',
  space_time = TIME_SPACE$space,
  linear_angular = LINEAR_ANGULAR$linear,
  rate_extent = EXTENT_RATE$extent,
  unit = "m",
  unit_latex = "\\text{m}",
  symbol = "\u03BB",
  symbol_latex = "\\lambda",
  symbol_expression = "italic(λ)",
  name = "linear wavelength"
)

LINEAR_WAVENUMBER <- data.frame(
  class_name = 'linear_wavenumber',
  space_time = TIME_SPACE$space,
  linear_angular = LINEAR_ANGULAR$linear,
  rate_extent = EXTENT_RATE$rate,
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

# Define the assemble_label function
assemble_label <- function(property) {
  paste0(
    'atop(',
    property$symbol_expression, ', ',
    'atop("', property$name, '", ',
    '"[', property$linear_angular, ' ', property$space_time, ' ', property$rate_extent, ']"))'
  )
}

# Use lapply to iterate over the rows of PROPERTIES as lists
PROPERTY_NODES <- data.frame(
  name = PROPERTIES$class_name,
  label = lapply(1:nrow(PROPERTIES), function(i) assemble_label(as.list(PROPERTIES[i, ]))) %>% unlist(),
  x = c(1, 1, 0, 0, 3, 3, 2, 2),  # Adjusted x-coordinates for a cube structure
  y = c(3, 1, 2, 0, 3, 1, 2, 0)   # Adjusted y-coordinates for a cube structure
)

# Function to find mismatches in dimensions
find_mismatches <- function(target_row, nodes, dimension_names) {
  apply(nodes, 1, function(row) {
    # Identify differing dimensions
    diff_cols <- dimension_names[row != target_row]
    if (length(diff_cols) == 1) {
      # Return the row index and differing dimension values
      list(
        row_index = which(apply(nodes, 1, function(x) all(x == row))),
        from_dimension = as.character(target_row[diff_cols]),
        to_dimension = as.character(row[diff_cols])
      )
    } else {
      NULL
    }
  }) %>% purrr::compact() # Remove NULL values
}

# Use property dimensions to build the edges
PROPERTY_DIMENSIONS <- PROPERTIES[, names(DIMENSIONS), drop = FALSE]
row.names(PROPERTY_DIMENSIONS) <- NULL

# Use property dimensions to build the edges
EDGE_DIMENSION_IDS <- do.call(rbind, lapply(1:nrow(PROPERTY_DIMENSIONS), function(i) {
  # Current row as the target row
  target_row <- PROPERTY_DIMENSIONS[i, ]

  # Find mismatches
  mismatches <- find_mismatches(target_row, PROPERTY_DIMENSIONS, names(DIMENSIONS))

  # Construct a data frame with from, to, and mismatched dimensions
  do.call(rbind, lapply(mismatches, function(mismatch) {
    data.frame(
      from = i,
      to = mismatch$row_index,
      from_dimension = mismatch$from_dimension,
      to_dimension = mismatch$to_dimension,
      stringsAsFactors = FALSE
    )
  }))
}))

inverted_direction <- function(from, to) {
  from_index <- which(unlist(DIMENSIONS) == from)
  to_index <- which(unlist(DIMENSIONS) == to)
  from_index > to_index
}

relationship_expression <- function(from, to) {
  mapply(function(from, to) {
    if (inverted_direction(from,to)) {
      return(paste(to, '%<-%', from))
    } else {
      return(paste(from, '%->%', to))
    }
  }, from, to, USE.NAMES = FALSE)  # Disable automatic naming
}


EX_C_X = 'c %.% x'
DF_C_X = function(x) {DEFAULT_SPEED_OF_MEDIUM * x}

EX_X_OVER_C = 'x / c'
DF_X_OVER_C = function(x) {x / DEFAULT_SPEED_OF_MEDIUM}

EX_2PI_X = '2 * pi %.% x'
DF_2PI_X = function(x) {2 * pi * x}

EX_X_OVER_2PI = 'x / ( 2 * pi )'
DF_X_OVER_2PI = function(x) {x / (2 * pi)}

EX_1_OVER_X = '1 / x'
DF_1_OVER_X = function(x) {1 * x}


# TODO:
# 1. Get existing tests to pass
# 2. Add this stuff manual stuff below into tests
# 3. Get rid of this stuff in the code by using PROPERTIES[5,] style code to pass everyhthing into the lookup
#       * EXTENT_RATE is easy all are 1 / x
#       * LINEAR_ANGULAR is half as easy: half are x / 2 pi and the other half are 2 pi x depending on direction
#       * TIME_SPACE is a quarter easy : they depend on direction and whether they are rate or extent. c x or x / c.


TRANSFORM_FUNCTIONS <- tibble::tribble(
  ~from,                        ~to,                            ~function_definition, ~function_expression,

  # Space Time Transforms

  LINEAR_WAVENUMBER$class_name,  LINEAR_FREQUENCY$class_name,   DF_C_X,               EX_C_X,
  LINEAR_PERIOD$class_name,      LINEAR_WAVELENGTH$class_name,  DF_C_X,               EX_C_X,
  ANGULAR_WAVENUMBER$class_name, ANGULAR_FREQUENCY$class_name,  DF_C_X,               EX_C_X,
  ANGULAR_PERIOD$class_name,     ANGULAR_WAVELENGTH$class_name, DF_C_X,               EX_C_X,

  LINEAR_FREQUENCY$class_name,   LINEAR_WAVENUMBER$class_name,  DF_X_OVER_C,          EX_X_OVER_C,
  LINEAR_WAVELENGTH$class_name,  LINEAR_PERIOD$class_name,      DF_X_OVER_C,          EX_X_OVER_C,
  ANGULAR_FREQUENCY$class_name,  ANGULAR_WAVENUMBER$class_name, DF_X_OVER_C,          EX_X_OVER_C,
  ANGULAR_WAVELENGTH$class_name, ANGULAR_PERIOD$class_name,     DF_X_OVER_C,          EX_X_OVER_C,

  # Linear Angular Transforms

  LINEAR_FREQUENCY$class_name,   ANGULAR_FREQUENCY$class_name,  DF_2PI_X,             EX_2PI_X,
  LINEAR_PERIOD$class_name,      ANGULAR_PERIOD$class_name,     DF_2PI_X,             EX_2PI_X,
  LINEAR_WAVENUMBER$class_name,  ANGULAR_WAVENUMBER$class_name, DF_2PI_X,             EX_2PI_X,
  LINEAR_WAVELENGTH$class_name,  ANGULAR_WAVELENGTH$class_name, DF_2PI_X,             EX_2PI_X,

  ANGULAR_FREQUENCY$class_name,  LINEAR_FREQUENCY$class_name,   DF_X_OVER_2PI,        EX_X_OVER_2PI,
  ANGULAR_PERIOD$class_name,     LINEAR_PERIOD$class_name,      DF_X_OVER_2PI,        EX_X_OVER_2PI,
  ANGULAR_WAVENUMBER$class_name, LINEAR_WAVENUMBER$class_name,  DF_X_OVER_2PI,        EX_X_OVER_2PI,
  ANGULAR_WAVELENGTH$class_name, LINEAR_WAVELENGTH$class_name,  DF_X_OVER_2PI,        EX_X_OVER_2PI,

  # Extent Rate Transforms

  LINEAR_WAVENUMBER$class_name,  LINEAR_WAVELENGTH$class_name,  DF_1_OVER_X,          EX_1_OVER_X,
  LINEAR_FREQUENCY$class_name,   LINEAR_PERIOD$class_name,      DF_1_OVER_X,          EX_1_OVER_X,
  LINEAR_WAVELENGTH$class_name,  LINEAR_WAVENUMBER$class_name,  DF_1_OVER_X,          EX_1_OVER_X,
  LINEAR_PERIOD$class_name,      LINEAR_FREQUENCY$class_name,   DF_1_OVER_X,          EX_1_OVER_X,

  ANGULAR_WAVENUMBER$class_name, ANGULAR_WAVELENGTH$class_name, DF_1_OVER_X,          EX_1_OVER_X,
  ANGULAR_FREQUENCY$class_name,  ANGULAR_PERIOD$class_name,     DF_1_OVER_X,          EX_1_OVER_X,
  ANGULAR_WAVELENGTH$class_name, ANGULAR_WAVENUMBER$class_name, DF_1_OVER_X,          EX_1_OVER_X,
  ANGULAR_PERIOD$class_name,     ANGULAR_FREQUENCY$class_name,  DF_1_OVER_X,          EX_1_OVER_X
)

function_expression <- function(from, to) {
  mapply(function(f, t) {
    match_row <- TRANSFORM_FUNCTIONS %>% subset(from == f & to == t)
    match_row$function_expression
  }, from, to, USE.NAMES = FALSE)  # Disable automatic naming
}

function_definition <- function(from, to) {
  mapply(function(f, t) {
    match_row <- TRANSFORM_FUNCTIONS %>% subset(from == f & to == t)
    match_row$function_definition
  }, from, to, SIMPLIFY = FALSE, USE.NAMES = FALSE)  # Disable automatic naming
}

# Create the EDGE_DIMENSIONS table using EDGE_DIMENSION_IDS
PROPERTY_EDGES <- data.frame(
  from = PROPERTIES$class_name[EDGE_DIMENSION_IDS$from],
  to   = PROPERTIES$class_name[EDGE_DIMENSION_IDS$to],
  relationship_expression = relationship_expression(
    EDGE_DIMENSION_IDS$from_dimension,
    EDGE_DIMENSION_IDS$to_dimension
  ),
  function_expression = function_expression(
    PROPERTIES$class_name[EDGE_DIMENSION_IDS$from],
    PROPERTIES$class_name[EDGE_DIMENSION_IDS$to]
  )
)

# Add arc_label column by pasting two existing columns
PROPERTY_EDGES$arc_expression <- paste(
  PROPERTY_EDGES$relationship_expression,
  ' ~ ~ (',
  PROPERTY_EDGES$function_expression,
  ')'
)

PROPERTY_EDGES$function_definition <- function_definition(
  PROPERTY_EDGES$from,
  PROPERTY_EDGES$to
)

# Create the graph as an undirected graph
PROPERTY_RELATIONSHIPS <- igraph::graph_from_data_frame(
  d = PROPERTY_EDGES,
  vertices = PROPERTY_NODES,
  directed = T
)

PROPERTY_RELATIONSHIPS_PLOT <- ggraph::ggraph(PROPERTY_RELATIONSHIPS,
                                              layout = "manual",
                                              x = PROPERTY_NODES$x,
                                              y = PROPERTY_NODES$y) +
  ggraph::geom_edge_link(
    edge_width = 0.8,
    color = "gray20"
  ) +
  ggraph::geom_edge_arc(
    ggplot2::aes(label = arc_expression),  # Label as aesthetic
    angle_calc = 'along',
    arrow = ggplot2::arrow(length = ggplot2::unit(4, "mm"), type = "closed"),  # Arrowheads for directed edges
    start_cap = ggraph::circle(2, 'mm'),  # Start offset for arcs
    end_cap = ggraph::circle(2, 'mm'),    # End offset for arcs
    edge_width = 0.8,
    strength = 0.2,
    color = "gray",  # Set arcs to gray
    label_parse = TRUE,
    label_size = 3,
    vjust = -0.5,  # Adjust label placement
    label_colour = "black"  # Set labels to black for contrast
  ) +
  # Add PROPERTY_NODES with light blue color
  ggraph::geom_node_point(size = 8, color = "darkgray") +
  # Add node labels
  ggraph::geom_node_text(
    ggplot2::aes(label = label),
    parse = TRUE,
    nudge_y = 0.3,  # Offset node labels slightly
    nudge_x = -0.05,  # Offset node labels slightly
    size = 4
  ) +
  # Add title and expand the plot space
  ggplot2::ggtitle("Wave Properties") +
  ggraph::theme_graph(base_family = "sans") +
  ggplot2::theme(plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.2)) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.2)) +
  ggplot2::coord_fixed(ratio = 1)  # Ensure equal aspect ratio for x and y axes


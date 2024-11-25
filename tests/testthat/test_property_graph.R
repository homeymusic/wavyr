# Test 1: Check that the graph has 8 nodes and 12 edges
test_that("graph has correct number of nodes and edges", {
  graph_order_value <- igraph::gorder(PROPERTY_RELATIONSHIPS)  # Number of nodes
  graph_size_value <- igraph::gsize(PROPERTY_RELATIONSHIPS)    # Number of edges

  expect_equal(graph_order_value, 8)  # Should have 8 nodes
  expect_equal(graph_size_value, 24)  # Should have 12 edges
})

# Test 2: Check that path_length centrality is calculated correctly
test_that("path_length centrality calculation works", {
  centrality_scores <- igraph::degree(PROPERTY_RELATIONSHIPS)

  expect_true(all(!is.na(centrality_scores)))  # All scores should be non-NA
  expect_true(all(centrality_scores >= 0))    # path_length centrality should be non-negative
})

# Test 3: Verify that the graph is connected (should be a single component)
test_that("graph is connected", {
  is_connected <- igraph::is_connected(PROPERTY_RELATIONSHIPS)

  expect_true(is_connected)  # Should return TRUE if the graph is connected
})

# Test 4: Check graph diameter
test_that("graph diameter is computed correctly", {
  diameter <- igraph::diameter(PROPERTY_RELATIONSHIPS)

  expect_equal(diameter, 3)  # Expected diameter value
})

# Test 5: Check if the graph has correct number of components
test_that("graph has correct number of components", {
  num_components <- igraph::components(PROPERTY_RELATIONSHIPS)$no

  expect_equal(num_components, 1)  # Expecting 1 connected component
})

# Test 6: Ensure all nodes are unique in the graph
test_that("all nodes are unique", {
  node_names <- igraph::V(PROPERTY_RELATIONSHIPS)$name

  expect_equal(length(node_names), length(unique(node_names)))  # No duplicate nodes
})

# Test 7: Check node path_length (number of connections) for each node
test_that("node path_length calculation is correct", {
  node_path_lengths <- igraph::degree(PROPERTY_RELATIONSHIPS)

  expect_true(all(node_path_lengths >= 0))  # path_length should be non-negative
})

# Test 8: path_length 0 should return nodes paired with themselves
test_that("path_length 0 returns nodes paired with themselves", {
  result <- filter_graph_by(0)
  expect_equal(nrow(result), 8)
  expected_pairs <- data.frame(
    from = c("linear_frequency", "linear_period", "linear_wavenumber", "linear_wavelength",
             "angular_frequency", "angular_period", "angular_wavenumber", "angular_wavelength"),
    to = c("linear_frequency", "linear_period", "linear_wavenumber", "linear_wavelength",
           "angular_frequency", "angular_period", "angular_wavenumber", "angular_wavelength"),
    path_length = 0
  )
  expect_equal(result, expected_pairs)
})

# Test 9: path_length 1 returns nodes directly connected by edges
test_that("path_length 1 returns nodes directly connected by edges", {
  result <- filter_graph_by(1) %>% dplyr::arrange(from)
  expect_equal(nrow(result), 24)

  expected_pairs <- data.frame(
    from = c("linear_frequency", "linear_frequency", "linear_frequency", "linear_period", "linear_period", "linear_period",
             "linear_wavenumber", "linear_wavenumber", "linear_wavenumber", "linear_wavelength", "linear_wavelength", "linear_wavelength",
             "angular_frequency", "angular_frequency", "angular_frequency", "angular_period", "angular_period", "angular_period",
             "angular_wavenumber", "angular_wavenumber", "angular_wavenumber", "angular_wavelength", "angular_wavelength", "angular_wavelength"),
    to = c("linear_period", "linear_wavenumber", "angular_frequency", "linear_frequency", "linear_wavelength", "angular_period",
           "linear_frequency", "linear_wavelength", "angular_wavenumber", "linear_period", "linear_wavenumber", "angular_wavelength",
           "linear_frequency", "angular_period", "angular_wavenumber", "linear_period", "angular_frequency", "angular_wavelength",
           "linear_wavenumber", "angular_frequency", "angular_wavelength", "linear_wavelength", "angular_period", "angular_wavenumber"),
    path_length = 1
  )  %>% dplyr::arrange(from)

  expect_equal(result, expected_pairs)
})

# Test 10: path_length 2 nodes should return nodes that are 2 edges apart
test_that("path_length 2 returns nodes 2 steps apart", {
  result <- filter_graph_by(2) %>% dplyr::arrange(from)

  expect_equal(nrow(result), 24)

  expected_pairs <- data.frame(
    from = c("linear_frequency", "linear_frequency", "linear_frequency", "linear_period", "linear_period", "linear_period",
             "linear_wavenumber", "linear_wavenumber", "linear_wavenumber", "linear_wavelength", "linear_wavelength", "linear_wavelength",
             "angular_frequency", "angular_frequency", "angular_frequency", "angular_period", "angular_period", "angular_period",
             "angular_wavenumber", "angular_wavenumber", "angular_wavenumber", "angular_wavelength", "angular_wavelength", "angular_wavelength"),
    to = c(
      "linear_wavelength", "angular_period", "angular_wavenumber", "linear_wavenumber",
      "angular_frequency", "angular_wavelength", "linear_period", "angular_frequency",
      "angular_wavelength", "linear_frequency", "angular_period", "angular_wavenumber",
      "linear_period", "linear_wavenumber", "angular_wavelength", "linear_frequency",
      "linear_wavelength", "angular_wavenumber", "linear_frequency", "linear_wavelength",
      "angular_period", "linear_period", "linear_wavenumber", "angular_frequency"
    ),
    path_length = 2
  ) %>% dplyr::arrange(from)


  expect_equal(result, expected_pairs)
})

# Test 11: path_length 3 returns nodes 3 steps apart
test_that("path_length 3 returns nodes 3 steps apart", {

  result <- filter_graph_by(3) %>% dplyr::arrange(from)
  expect_equal(nrow(result), 8)

  expected_pairs <- data.frame(
    from = c("linear_frequency", "linear_period", "linear_wavenumber", "linear_wavelength",
             "angular_frequency", "angular_period", "angular_wavenumber", "angular_wavelength"),
    to = c("angular_wavelength", "angular_wavenumber", "angular_period", "angular_frequency",
           "linear_wavelength", "linear_wavenumber", "linear_period", "linear_frequency"),
    path_length = 3
  ) %>% dplyr::arrange(from)

  expect_equal(result, expected_pairs)
})

# Test 12: Graph plot of all relationships renders correctly
test_that("graph plot of all relationships renders correctly", {
  vdiffr::expect_doppelganger(
    "PROPERTY_RELATIONSHIPS_PLOT",
    PROPERTY_RELATIONSHIPS_PLOT
  )
})

test_that("path_length 1 with relationships 'LINEAR_ANGULAR' returns the correct node pairs and column names", {
  result <- filter_graph_by(path_length = 1, relationships = c(LINEAR_ANGULAR$label)) %>% dplyr::arrange(from)

  # Check that the column names are correct
  expect_equal(colnames(result), c("from", "to", "path_length"))

  expect_equal(nrow(result), 24)

  # Expected node pairs
  expected_pairs <- data.frame(
    from = c(
      "angular_frequency", "angular_frequency", "angular_frequency", "angular_period", "angular_period",
      "angular_period", "angular_wavelength", "angular_wavelength", "angular_wavelength", "angular_wavenumber",
      "angular_wavenumber", "angular_wavenumber", "linear_frequency", "linear_frequency", "linear_frequency",
      "linear_period", "linear_period", "linear_period", "linear_wavelength", "linear_wavelength",
      "linear_wavelength", "linear_wavenumber", "linear_wavenumber", "linear_wavenumber"
    ),
    to = c(
      "linear_frequency", "angular_period", "angular_wavenumber", "linear_period", "angular_frequency",
      "angular_wavelength", "linear_wavelength", "angular_period", "angular_wavenumber", "linear_wavenumber",
      "angular_frequency", "angular_wavelength", "linear_period", "linear_wavenumber", "angular_frequency",
      "linear_frequency", "linear_wavelength", "angular_period", "linear_period", "linear_wavenumber",
      "angular_wavelength", "linear_frequency", "linear_wavelength", "angular_wavenumber"
    ),
    path_length = rep(1, 24)  # Assuming path_length 1
  ) %>% dplyr::arrange(from)

  # Check if the result matches the expected node pairs
  expect_equal(result, expected_pairs)
})

test_that("path_length 2 with relationships 'LINEAR_ANGULAR' and 'EXTENT_RATE' returns the correct node pairs and column names", {
  relationships = c(LINEAR_ANGULAR$label, EXTENT_RATE$label)
  result <- filter_graph_by(path_length = 2, relationships = relationships) %>% dplyr::arrange(from)

  # Check that the column names are correct
  expect_equal(colnames(result), c("from", "to", "path_length"))

  expect_equal(nrow(result), 24)  # Adjust based on expected results

  # Expected node pairs for path_length 2 and relationships 'LINEAR_ANGULAR' and 'EXTENT_RATE'
  expected_pairs <- data.frame(
    from = c(
      "angular_frequency", "angular_frequency", "angular_frequency", "angular_period", "angular_period",
      "angular_period", "angular_wavelength", "angular_wavelength", "angular_wavelength", "angular_wavenumber",
      "angular_wavenumber", "angular_wavenumber", "linear_frequency", "linear_frequency", "linear_frequency",
      "linear_period", "linear_period", "linear_period", "linear_wavelength", "linear_wavelength",
      "linear_wavelength", "linear_wavenumber", "linear_wavenumber", "linear_wavenumber"
    ),
    to = c(
      "linear_period", "linear_wavenumber", "angular_wavelength", "linear_frequency", "linear_wavelength",
      "angular_wavenumber", "linear_period", "linear_wavenumber", "angular_frequency", "linear_frequency",
      "linear_wavelength", "angular_period", "linear_wavelength", "angular_period", "angular_wavenumber",
      "linear_wavenumber", "angular_frequency", "angular_wavelength", "linear_frequency", "angular_period",
      "angular_wavenumber", "linear_period", "angular_frequency", "angular_wavelength"
    ),
    path_length = rep(2, 24)  # Assuming path_length 2
  ) %>% dplyr::arrange(from)

  # Check if the result matches the expected node pairs
  expect_equal(result, expected_pairs)
})

test_that("path_length 3 with relationships 'LINEAR_ANGULAR', 'EXTENT_RATE', and 'SPACE_TIME' returns the correct node pairs and column names", {
  relationships = c(LINEAR_ANGULAR$label, EXTENT_RATE$label, SPACE_TIME$label)
  result <- filter_graph_by(path_length = 3, relationships = relationships) %>% dplyr::arrange(from)

  # Check that the column names are correct
  expect_equal(colnames(result), c("from", "to", "path_length"))

  # Check the number of rows matches the expected results
  expect_equal(nrow(result), 8)

  # Expected node pairs for path_length 3 and relationships 'LINEAR_ANGULAR', 'EXTENT_RATE', and 'SPACE_TIME'
  expected_pairs <- data.frame(
    from = c("angular_frequency", "angular_period", "angular_wavelength", "angular_wavenumber",
             "linear_frequency", "linear_period", "linear_wavelength", "linear_wavenumber"),
    to = c("linear_wavelength", "linear_wavenumber", "linear_frequency", "linear_period",
           "angular_wavelength", "angular_wavenumber", "angular_frequency", "angular_period"),
    path_length = rep(3, 8)
  ) %>% dplyr::arrange(from)

  # Check if the result matches the expected node pairs
  expect_equal(result, expected_pairs)
})

test_that("path_length -1 for all returns nodes directly connected by edges", {
  result <- filter_graph_by(path_length = -1)

  expect_equal(nrow(result), 0)
})

test_that("path_length 4 for all returns nodes directly connected by edges", {
  result <- filter_graph_by(path_length = 4)

  expect_equal(nrow(result), 0)
})

expected_edge_metadata <- tibble::tribble(
  ~from,                        ~to,                            ~function_definition, ~function_expression, ~relationship_expression,

  # Space Time Transforms

  LINEAR_WAVENUMBER,  LINEAR_FREQUENCY,   DF_C_X,               EX_C_X,               'space %->% time',
  LINEAR_PERIOD,      LINEAR_WAVELENGTH,  DF_C_X,               EX_C_X,               'space %<-% time',
  ANGULAR_WAVENUMBER, ANGULAR_FREQUENCY,  DF_C_X,               EX_C_X,               'space %->% time',
  ANGULAR_PERIOD,     ANGULAR_WAVELENGTH, DF_C_X,               EX_C_X,               'space %<-% time',

  LINEAR_FREQUENCY,   LINEAR_WAVENUMBER,  DF_X_OVER_C,          EX_X_OVER_C,          'space %<-% time',
  LINEAR_WAVELENGTH,  LINEAR_PERIOD,      DF_X_OVER_C,          EX_X_OVER_C,          'space %->% time',
  ANGULAR_FREQUENCY,  ANGULAR_WAVENUMBER, DF_X_OVER_C,          EX_X_OVER_C,          'space %<-% time',
  ANGULAR_WAVELENGTH, ANGULAR_PERIOD,     DF_X_OVER_C,          EX_X_OVER_C,          'space %->% time',

  # Linear Angular Transforms

  # Linear to Angular
  LINEAR_FREQUENCY,   ANGULAR_FREQUENCY,  DF_2PI_X,             EX_2PI_X,             'linear %->% angular',
  LINEAR_PERIOD,      ANGULAR_PERIOD,     DF_X_OVER_2PI,        EX_X_OVER_2PI,        'linear %->% angular',
  LINEAR_WAVENUMBER,  ANGULAR_WAVENUMBER, DF_2PI_X,             EX_2PI_X,             'linear %->% angular',
  LINEAR_WAVELENGTH,  ANGULAR_WAVELENGTH, DF_X_OVER_2PI,        EX_X_OVER_2PI,        'linear %->% angular',

  # Angular to Linear
  ANGULAR_FREQUENCY,  LINEAR_FREQUENCY,   DF_X_OVER_2PI,        EX_X_OVER_2PI,        'linear %<-% angular',
  ANGULAR_PERIOD,     LINEAR_PERIOD,      DF_2PI_X,             EX_2PI_X,             'linear %<-% angular',
  ANGULAR_WAVENUMBER, LINEAR_WAVENUMBER,  DF_X_OVER_2PI,        EX_X_OVER_2PI,        'linear %<-% angular',
  ANGULAR_WAVELENGTH, LINEAR_WAVELENGTH,  DF_2PI_X,             EX_2PI_X,             'linear %<-% angular',

  # Extent Rate Transforms

  LINEAR_WAVENUMBER,  LINEAR_WAVELENGTH,  DF_1_OVER_X,          EX_1_OVER_X,          'extent %<-% rate',
  LINEAR_FREQUENCY,   LINEAR_PERIOD,      DF_1_OVER_X,          EX_1_OVER_X,          'extent %<-% rate',
  LINEAR_WAVELENGTH,  LINEAR_WAVENUMBER,  DF_1_OVER_X,          EX_1_OVER_X,          'extent %->% rate',
  LINEAR_PERIOD,      LINEAR_FREQUENCY,   DF_1_OVER_X,          EX_1_OVER_X,          'extent %->% rate',

  ANGULAR_WAVENUMBER, ANGULAR_WAVELENGTH, DF_1_OVER_X,          EX_1_OVER_X,          'extent %<-% rate',
  ANGULAR_FREQUENCY,  ANGULAR_PERIOD,     DF_1_OVER_X,          EX_1_OVER_X,          'extent %<-% rate',
  ANGULAR_WAVELENGTH, ANGULAR_WAVENUMBER, DF_1_OVER_X,          EX_1_OVER_X,          'extent %->% rate',
  ANGULAR_PERIOD,     ANGULAR_FREQUENCY,  DF_1_OVER_X,          EX_1_OVER_X,          'extent %->% rate'
)

# Strip environments from functions
strip_env <- function(func_list) {
  lapply(func_list, function(f) {
    environment(f) <- baseenv()  # Assign a common environment
    f
  })
}

test_that('metadata on edges are good', {

  expected_from <- expected_edge_metadata$from %>% dplyr::bind_rows()
  expected_to <- expected_edge_metadata$to %>% dplyr::bind_rows()

  # Test relationship_expression
  expected_relationship_expression <- expected_edge_metadata$relationship_expression
  actual_relationship_expression <- relationship_expression(expected_from, expected_to)
  expect_equal(
    actual_relationship_expression,
    expected_relationship_expression,
    label = "relationship_expression matches expected values"
  )

  # Test function_expression
  expected_function_expression <- expected_edge_metadata$function_expression
  actual_function_expression <- function_expression(expected_from, expected_to)
  expect_equal(
    actual_function_expression,
    expected_function_expression,
    label = "function_expression matches expected values"
  )

  expected_function_definition <- expected_edge_metadata$function_definition
  actual_function_definition   <- function_definition(expected_from, expected_to)

  stripped_actual   <- strip_env(actual_function_definition)
  stripped_expected <- strip_env(expected_function_definition)

  expect_equal(
    stripped_actual,
    stripped_expected,
    label = "function_definition matches expected values (ignoring environments)"
  )
})


# Test 1: Check that the graph has 8 nodes and 12 edges
test_that("graph has correct number of nodes and edges", {
  graph_order_value <- igraph::gorder(PROPERTY_RELATIONSHIPS)  # Number of nodes
  graph_size_value <- igraph::gsize(PROPERTY_RELATIONSHIPS)    # Number of edges

  expect_equal(graph_order_value, 8)  # Should have 8 nodes
  expect_equal(graph_size_value, 12)  # Should have 12 edges
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

  expect_equal(nrow(result), 8)

  # Expected node pairs
  expected_pairs <- data.frame(
    from = c("linear_frequency", "linear_wavenumber", "linear_period", "linear_wavelength",
             "angular_frequency", "angular_wavenumber", "angular_period", "angular_wavelength"),
    to = c("angular_frequency", "angular_wavenumber", "angular_period", "angular_wavelength",
           "linear_frequency", "linear_wavenumber", "linear_period", "linear_wavelength"),
    path_length = rep(1, 8)  # Assuming path_length 1
  ) %>% dplyr::arrange(from)

  # Check if the result matches the expected node pairs
  expect_equal(result, expected_pairs)
})

test_that("path_length 2 with relationships 'LINEAR_ANGULAR' and 'RATE_EXTENT' returns the correct node pairs and column names", {
  relationships = c(LINEAR_ANGULAR$label, RATE_EXTENT$label)
  result <- filter_graph_by(path_length = 2, relationships = relationships) %>% dplyr::arrange(from)

  # Check that the column names are correct
  expect_equal(colnames(result), c("from", "to", "path_length"))

  expect_equal(nrow(result), 8)  # Adjust based on expected results

  # Expected node pairs for path_length 2 and relationships 'LINEAR_ANGULAR' and 'RATE_EXTENT'
  expected_pairs <- data.frame(
    from = c("linear_frequency", "linear_period", "linear_wavenumber", "linear_wavelength",
             "angular_period", "angular_frequency", "angular_wavelength", "angular_wavenumber"),
    to = c("angular_period", "angular_frequency", "angular_wavelength", "angular_wavenumber",
           "linear_frequency", "linear_period", "linear_wavenumber", "linear_wavelength"),
    path_length = rep(2, 4)  # Assuming path_length 2
  ) %>% dplyr::arrange(from)

  # Check if the result matches the expected node pairs
  expect_equal(result, expected_pairs)
})

test_that("path_length 3 with relationships 'LINEAR_ANGULAR', 'RATE_EXTENT', and 'SPACE_TIME' returns the correct node pairs and column names", {
  relationships = c(LINEAR_ANGULAR$label, RATE_EXTENT$label, SPACE_TIME$label)
  result <- filter_graph_by(path_length = 3, relationships = relationships) %>% dplyr::arrange(from)

  # Check that the column names are correct
  expect_equal(colnames(result), c("from", "to", "path_length"))

  # Check the number of rows matches the expected results
  expect_equal(nrow(result), 8)

  # Expected node pairs for path_length 3 and relationships 'LINEAR_ANGULAR', 'RATE_EXTENT', and 'SPACE_TIME'
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

test_that("we can convert following the shortest path in the graph",{
  conversion = 1 %>% convert_from_to(
    'linear_frequency','angular_wavelength'
  )
  expect_equal(conversion$edge_path_length, 3)
  expect_equal(class(conversion$edge_path), "igraph.es")
  expect_equal(conversion$edge_path %>% as.numeric(), c(1,10,8))
  expect_equal(conversion$function_label, c(
    "1 / x",
    "x / c",
    "x / (2 * pi)"
  ))
  expect_equal(conversion$value, 8)
})

test_that('arrows makes sense for edge labels',{
  from = c('rate', 'extent')
  to = c('extent', 'rate')
  expect_equal(
    edge_expression(from, to),
    c('extent %<-% rate','extent %->% rate')
  )
})

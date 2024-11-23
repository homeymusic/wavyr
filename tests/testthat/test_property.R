test_that("property class has required parameter value, correct class, and default interval variables", {
  # Create a property object with the required parameter value
  p <- property(10)

  # Check if the object is of class "property"
  expect_equal(class(p), "property")

  # Test if the value is set correctly
  expect_equal(p$value, 10)

  # Test that 'value' is required and throws an error if missing
  expect_error(property(), "`x` must be numeric or a property object")

})

test_that("property class accepts another property object as input", {
  # Create a base property object
  base_property <- property(20)

  expect_equal(base_property$value, 20)

  # Create a new property using the base property as input
  new_property <- property(base_property)

  # Check if the new object is of class "property"
  expect_equal(class(new_property), "property")

  # Check if the value is correctly inherited from the base property
  expect_equal(new_property$value, 20)
})

test_that("property class throws an error if input is not numeric or property class", {
  # Attempt to create a property object with an invalid input
  expect_error(property(c(10, 20)), "`x` must be of length 1")
  expect_error(property(list(10)), "x` must be numeric or a property object")
  expect_error(property("string"), "`x` must be numeric or a property object")
})

test_that("property class throws an error if input has length greater than 1", {
  # Attempt to create a property object with a numeric vector
  expect_error(property(c(10, 20)), "`x` must be of length 1")

  # Attempt to create a property object with another property of invalid length
  invalid_property <- structure(
    list(value = c(10, 20)),
    class = "property"
  )
  expect_error(property(invalid_property), "`x` must be of length 1")
})

test_that('all subclasses appear on property', {
  expect_equal(PROPERTIES %>% names(), c(
    'angular_frequency',
    'angular_period',
    'angular_wavelength',
    'angular_wavenumber',
    'linear_frequency',
    'linear_period',
    'linear_wavelength',
    'linear_wavenumber'
  ))
})

#####
#####
#####

# Test 1: Check that the graph has 8 nodes and 12 edges
test_that("graph has correct number of nodes and edges", {
  # Extract the number of nodes (graph order)
  graph_order_value <- (PROPERTY_RELATIONSHIPS %>%
                          tidygraph::activate(nodes) %>%
                          tidygraph::mutate(graph_order = tidygraph::graph_order()) %>%
                          tidygraph::pull(graph_order))[1]  # Extract the first value

  # Extract the number of edges (graph size)
  graph_size_value <- (PROPERTY_RELATIONSHIPS %>%
                         tidygraph::activate(edges) %>%
                         tidygraph::mutate(graph_size = tidygraph::graph_size()) %>%
                         tidygraph::pull(graph_size))[1]  # Extract the first value

  # Assert the graph has the expected number of nodes and edges
  expect_equal(graph_order_value, 8)  # Should have 8 nodes
  expect_equal(graph_size_value, 12)  # Should have 12 edges
})

# Test 2: Check that degree centrality is calculated correctly
test_that("degree centrality calculation works", {
  # Calculate degree centrality for each node
  centrality_scores <- (PROPERTY_RELATIONSHIPS %>%
                          tidygraph::activate(nodes) %>%
                          tidygraph::mutate(degree_centrality = tidygraph::centrality_degree()) %>%
                          tidygraph::pull(degree_centrality))

  # Test if degree centrality scores are calculated (expecting numerical values)
  expect_true(all(!is.na(centrality_scores)))
  expect_true(all(centrality_scores >= 0))  # Degree centrality should be non-negative
})

# Test 3: Verify that the graph is connected (should be a single component)
test_that("graph is connected", {
  # Check if the graph is connected (should be a single component)
  is_connected <- (PROPERTY_RELATIONSHIPS %>%
                     tidygraph::activate(nodes) %>%
                     tidygraph::mutate(is_connected = tidygraph::graph_is_connected()) %>%
                     tidygraph::pull(is_connected))[1]  # Extract the first value

  expect_true(is_connected)  # Should return TRUE if the graph is connected
})

# Test 4: Check graph diameter
test_that("graph diameter is computed correctly", {
  # Calculate the graph's diameter (longest shortest path between nodes)
  diameter <- (PROPERTY_RELATIONSHIPS %>%
                 tidygraph::activate(nodes) %>%
                 tidygraph::mutate(graph_diameter = tidygraph::graph_diameter()) %>%
                 tidygraph::pull(graph_diameter))[1]  # Extract the first value

  # Replace expect_is with expect_type for checking the type of 'diameter'
  expect_equal(diameter, 3)  # Ensure it's a positive number
})

# Test 5: Check if the graph has correct number of components
test_that("graph has correct number of components", {
  # Calculate the number of components in the graph
  num_components <- (PROPERTY_RELATIONSHIPS %>%
                       tidygraph::activate(nodes) %>%
                       tidygraph::mutate(num_components = tidygraph::group_components()) %>%
                       tidygraph::pull(num_components))[1]  # Extract the first value

  expect_equal(num_components, 1)  # Expecting 1 component in a connected graph
})

# Test 8: Ensure all nodes are unique in the graph
test_that("all nodes are unique", {
  node_names <- (PROPERTY_RELATIONSHIPS %>%
                   tidygraph::activate(nodes) %>%
                   tidygraph::pull(name))

  # Ensure there are no duplicates in node names
  expect_equal(length(node_names), length(unique(node_names)))
})

# Test 9: Check node degree (number of connections) for each node
test_that("node degree calculation is correct", {
  # Calculate degree for each node
  node_degrees <- (PROPERTY_RELATIONSHIPS %>%
                     tidygraph::activate(nodes) %>%
                     tidygraph::mutate(node_degree = tidygraph::centrality_degree()) %>%
                     tidygraph::pull(node_degree))

  # Ensure all node degrees are non-negative and correctly calculated
  expect_true(all(node_degrees >= 0))  # Degree should be >= 0
})

# Test 10: Test degree 0 should return nodes paired with themselves
test_that("degree 0 returns nodes paired with themselves", {
  result <- property_relationships(0)
  expected_pairs <- tibble::tibble(
    from = c("linear_frequency", "linear_period", "linear_wavenumber", "linear_wavelength",
             "angular_frequency", "angular_period", "angular_wavenumber", "angular_wavelength"),
    to = c("linear_frequency", "linear_period", "linear_wavenumber", "linear_wavelength",
           "angular_frequency", "angular_period", "angular_wavenumber", "angular_wavelength")
  )
  expect_equal(result, expected_pairs)
})

test_that("degree 1 returns nodes directly connected by edges", {
  result <- property_relationships(1)

  expected_pairs <- tibble::tibble(
    from = c("linear_frequency", "linear_frequency", "linear_frequency", "linear_period", "linear_period", "linear_period",
             "linear_wavenumber", "linear_wavenumber", "linear_wavenumber", "linear_wavelength", "linear_wavelength", "linear_wavelength",
             "angular_frequency", "angular_frequency", "angular_frequency", "angular_period", "angular_period", "angular_period",
             "angular_wavenumber", "angular_wavenumber", "angular_wavenumber", "angular_wavelength", "angular_wavelength", "angular_wavelength"),
    to = c("linear_period", "linear_wavenumber", "angular_frequency", "linear_frequency", "linear_wavelength", "angular_period",
           "linear_frequency", "linear_wavelength", "angular_wavenumber", "linear_period", "linear_wavenumber", "angular_wavelength",
           "linear_frequency", "angular_period", "angular_wavenumber", "linear_period", "angular_frequency", "angular_wavelength",
           "linear_wavenumber", "angular_frequency", "angular_wavelength", "linear_wavelength", "angular_period", "angular_wavenumber")
  )

  expect_equal(result %>% dplyr::select(from, to), expected_pairs)
})

# Test 7: Degree 2 nodes should return nodes that are 2 edges apart
test_that("degree 2 returns nodes 2 steps apart", {
  result <- property_relationships(2) %>% dplyr::select(from, to)

  expected_pairs <- tibble::tibble(
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
    )
  )

  expect_equal(result, expected_pairs)

})

test_that("degree 3 returns nodes 3 steps apart", {
  result <- property_relationships(3) %>% dplyr::select(from, to)

  expected_pairs <- tibble::tibble(
    from = c("linear_frequency", "linear_period", "linear_wavenumber", "linear_wavelength",
             "angular_frequency", "angular_period", "angular_wavenumber", "angular_wavelength"),
    to = c("angular_wavelength", "angular_wavenumber", "angular_period", "angular_frequency",
           "linear_wavelength", "linear_wavenumber", "linear_period", "linear_frequency")
  )

  expect_equal(result, expected_pairs)
})

test_that("graph plot of all relationships renders correctly", {
  vdiffr::expect_doppelganger(
    "PROPERTY_RELATIONSHIPS_plot",
    PROPERTY_RELATIONSHIPS_PLOT
  )
})

# Test 10: Test degree 0 should return nodes paired with themselves
test_that("degree 0 returns nodes paired with themselves", {
  result <- property_relationships(0, relationships = c())
  expected_pairs <- tibble::tibble(
    from = c("linear_frequency", "linear_period", "linear_wavenumber", "linear_wavelength",
             "angular_frequency", "angular_period", "angular_wavenumber", "angular_wavelength"),
    to = c("linear_frequency", "linear_period", "linear_wavenumber", "linear_wavelength",
           "angular_frequency", "angular_period", "angular_wavenumber", "angular_wavelength")
  )
  expect_equal(result, expected_pairs)
})


test_that("degree and length of relationships match if relationships is provided", {

  # Test case where relationships are provided but degree is 0
  expect_error(
    property_relationships(degree = 0, relationships = c("Measure", "Rotation")),
    "The length of relationships must match the degree."
  )

  expect_error(
    property_relationships(degree = 1, relationships = c("Measure", "Rotation")),
    "The length of relationships must match the degree."
  )

  expect_error(
    property_relationships(degree = 2, relationships = c("Measure")),
    "The length of relationships must match the degree."
  )

  expect_error(
    property_relationships(degree = 3, relationships = c("Measure")),
    "The length of relationships must match the degree."
  )

})

# test_that("degree 1 with relationships 'Rotation' returns the correct node pairs and column names", {
#   result <- property_relationships(degree = 1, relationships = c("Rotation"))
#
#   # Check that the column names are correct
#   expect_equal(colnames(result), c("from", "to", "order", "relationships"))
#
#   # Check that all entries in the 'relationships' column have the value 'Rotation'
#   expect_true(all(result$relationships == "Rotation"))
#
#   expect_equal(nrow(result), 4)
#
#   # Expected node pairs
#   expected_pairs <- tibble::tibble(
#     from = c("linear_frequency", "linear_wavenumber", "linear_period", "linear_wavelength"),
#     to = c("angular_frequency", "angular_wavenumber", "angular_period", "angular_wavelength"),
#     order = rep(1, 4),  # Assuming degree 1
#     relationships = rep("Rotation", 4)
#   )
#
#   # Check if the result matches the expected node pairs
#   expect_equal(result, expected_pairs)
# })
#
# test_that("degree 2 with relationships 'Rotation' and 'Measure' returns the correct node pairs and column names", {
#   relationships = c("Rotation", "Measure")
#   result <- property_relationships(degree = 2, relationships = relationships)
#
#   # Check that the column names are correct
#   expect_equal(colnames(result), c("from", "to", "order", "relationships"))
#
#   # Check that all entries in the 'relationships' column have the correct values
#   expect_true(all(result$relationships %in% relationships))
#
#   expect_equal(nrow(result), 4)  # Adjust based on expected results
#
#   # Expected node pairs for degree 2 and relationships 'Rotation' and 'Measure'
#   expected_pairs <- tibble::tibble(
#     from = c("linear_frequency", "linear_period", "linear_wavenumber", "linear_wavelength"),
#     to = c("angular_period", "angular_frequency", "angular_wavelength", "angular_wavenumber"),
#     order = rep(2, 4),  # Assuming degree 2
#     relationships = rep(list(relationships), 4)
#   )
#
#   # Check if the result matches the expected node pairs
#   expect_equal(result, expected_pairs)
# })

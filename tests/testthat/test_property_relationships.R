# Define expected properties for comparisons
f <- 440
w <- 2 * pi * f

expected_properties <- list(
  linear_frequency   = f,
  linear_period      = 1 / f,
  linear_wavenumber  = f / DEFAULT_SPEED_OF_MEDIUM,
  linear_wavelength  = DEFAULT_SPEED_OF_MEDIUM / f,
  angular_frequency  = w,
  angular_period     = 1 / w,
  angular_wavenumber = w / DEFAULT_SPEED_OF_MEDIUM,
  angular_wavelength = DEFAULT_SPEED_OF_MEDIUM / w
)

# Helper function to validate property conversions
validate_property_conversions <- function(result, expected_properties) {
  for (pair in 1:nrow(result)) {
    from_class <- result$from[pair]
    to_class <- result$to[pair]

    # Get the expected value for the 'from' property
    expected_from_value <- expected_properties[[from_class]]

    # Create the 'from' and 'to' objects using the class names and values
    from_obj <- do.call(from_class, list(expected_from_value))
    to_obj <- do.call(to_class, list(from_obj))

    # Get the expected value for the 'to' property
    expected_to_value <- expected_properties[[to_class]]

    # Validate the 'to' property value
    expect_equal(to_obj$value, expected_to_value, tolerance = 0.1,
                 info = paste("from:", from_class,
                              "to:", to_class))
  }
}

test_that('simple frequency conversion works', {
  f_obj = linear_frequency(expected_properties$linear_frequency)
  expect_equal(f_obj$value, expected_properties$linear_frequency)
  l_ang = angular_wavelength(f_obj)
  expect_equal(l_ang$value, expected_properties$angular_wavelength)
})

# Test for path_length 0 (nodes paired with themselves)
test_that("path_length 0 returns nodes paired with themselves", {
  # Get all the node pairs from path_length 0
  result <- filter_graph_by(0) %>%
    dplyr::select(from, to)

  # Validate the property conversions for path_length 0
  validate_property_conversions(result, expected_properties)
})

# Test for path_length 1 (nodes directly connected by edges)
test_that("path_length 1 for Rate ~ Extent returns nodes directly connected by edges", {
  relationships = c(EXTENT_RATE$label)

  result <- filter_graph_by(path_length = 1, relationships = relationships) %>% dplyr::arrange(from)

  expect_equal(nrow(result), 8)

  # Validate the property conversions for path_length 1
  validate_property_conversions(result, expected_properties)
})

test_that("angluar w to and from linear f works", {

  # w to f
  w = angular_frequency(expected_properties$angular_frequency)
  expect_equal(w$value, expected_properties$angular_frequency)

  f = linear_frequency(w)
  expect_equal(f$value, expected_properties$linear_frequency)

  # f to w
  f = linear_frequency(expected_properties$linear_frequency)
  expect_equal(f$value, expected_properties$linear_frequency)

  w = angular_frequency(f)
  expect_equal(w$value, expected_properties$angular_frequency)


})


test_that("angluar w to and from angular k works", {

  # w to k
  w = angular_frequency(expected_properties$angular_frequency)
  expect_equal(w$value, expected_properties$angular_frequency)

  k = angular_wavenumber(w)
  expect_equal(k$value, expected_properties$angular_wavenumber)

  # f to w
  k = angular_wavenumber(expected_properties$angular_wavenumber)
  expect_equal(k$value, expected_properties$angular_wavenumber)

  w = angular_frequency(k)
  expect_equal(w$value, expected_properties$angular_frequency)

})

# Test for path_length 1 (nodes directly connected by edges)
test_that("path_length 1 for Linear ~ Angular returns nodes directly connected by edges", {
  relationships = c(LINEAR_ANGULAR$label)

  result <- filter_graph_by(path_length = 1, relationships = relationships)

  expect_equal(nrow(result), 8)

  # Validate the property conversions for path_length 1
  validate_property_conversions(result, expected_properties)
})

# Test for path_length 1 (nodes directly connected by edges)
test_that("path_length 1 for Time ~ Space returns nodes directly connected by edges", {
  relationships = c(TIME_SPACE$label)

  result <- filter_graph_by(path_length = 1, relationships = relationships)

  expect_equal(nrow(result), 8)

  # Validate the property conversions for path_length 1
  validate_property_conversions(result, expected_properties)
})

# Test for path_length 1 (nodes directly connected by edges)
test_that("path_length 1 for all returns nodes directly connected by edges", {
  result <- filter_graph_by(path_length = 1)

  expect_equal(nrow(result), 24)

  # Validate the property conversions for path_length 1
  validate_property_conversions(result, expected_properties)
})

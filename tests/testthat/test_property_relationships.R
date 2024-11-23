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
    expect_equal(to_obj$value, expected_to_value, tolerance = 0.1)
  }
}

# Test for path_length 0 (nodes paired with themselves)
test_that("path_length 0 returns nodes paired with themselves", {
  # Get all the node pairs from path_length 0
  result <- property_relationships(0) %>%
    dplyr::select(from, to)

  # Validate the property conversions for path_length 0
  validate_property_conversions(result, expected_properties)
})

# # Test for path_length 1 (nodes directly connected by edges)
# test_that("path_length 1 returns nodes directly connected by edges", {
#   # Get all the node pairs from path_length 1
#   result <- property_relationships(1) %>%
#     dplyr::select(from, to)
#
#   # Validate the property conversions for path_length 1
#   validate_property_conversions(result, expected_properties)
# })

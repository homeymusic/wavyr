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

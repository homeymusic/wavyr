test_that("angular_wavelength subclass has correct classes", {
  # Create a angular_wavelength object
  l_a <- angular_wavelength(10)

  # Check that the object has the correct classes
  expect_true(inherits(l_a, "angular_wavelength"))
  expect_true(inherits(l_a, "property"))
  expect_equal(l_a$class_name, 'angular_wavelength')
  expect_equal(l_a$dimension, Dimension$spatial)
  expect_equal(l_a$rotation,  Rotation$angular)
  expect_equal(l_a$measure,   Measure$extent)

  # Check if the value is set correctly (from the `property` class)
  expect_equal(l_a$value, 10)

  # Check that the default unit, symbol, and name are inherited from `property`
  expect_equal(l_a$unit, "m/rad")
  expect_equal(l_a$unit_latex, "\\frac{\\text{m}}{\\text{rad}}")

  expect_equal(l_a$symbol, "l_angular")
  expect_equal(l_a$symbol_latex, "\\lambda_\\text{angular}")

  expect_equal(l_a$name, "angular wavelength")
})

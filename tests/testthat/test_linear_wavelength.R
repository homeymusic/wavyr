test_that("linear_wavelength subclass has correct classes", {
  # Create a linear_wavelength object
  l <- linear_wavelength(10)

  # Check that the object has the correct classes
  expect_true(inherits(l, "linear_wavelength"))
  expect_true(inherits(l, "property"))
  expect_equal(l$class_name, 'linear_wavelength')
  expect_equal(l$dimension, Dimension$spatial)
  expect_equal(l$rotation,  Rotation$linear)
  expect_equal(l$measure,   Measure$extent)

  # Check if the value is set correctly (from the `property` class)
  expect_equal(l$value, 10)

  expect_equal(l$unit, "m")
  expect_equal(l$unit_latex, "m")
  expect_equal(l$symbol, "\u03BB")
  expect_equal(l$symbol_latex, "\\lambda")
  expect_equal(l$name, "linear wavelength")
})

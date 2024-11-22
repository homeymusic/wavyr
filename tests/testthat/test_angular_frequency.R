test_that("angular_frequency subclass has correct classes", {
  # Create a angular_frequency object
  w <- angular_frequency(10)

  # Check that the object has the correct classes
  expect_true(inherits(w, "angular_frequency"))
  expect_true(inherits(w, "property"))
  expect_equal(w$class_name, 'angular_frequency')
  expect_equal(w$dimension, Dimension$temporal)
  expect_equal(w$rotation,  Rotation$angular)
  expect_equal(w$measure,   Measure$rate)

  # Check if the value is set correctly (from the `property` class)
  expect_equal(w$value, 10)

  # Check that the default unit, symbol, and name are inherited from `property`
  expect_equal(w$unit, "rad/s")
  expect_equal(w$unit_latex, "\\frac{\\text{rad}}{\\text{s}}")

  expect_equal(w$symbol, "\u03C9")
  expect_equal(w$symbol_latex, "\\omega")

  expect_equal(w$name, "angular frequency")
})

test_that("linear_frequency subclass has correct classes", {
  # Create a linear_frequency object
  f <- linear_frequency(10)

  # Check that the object has the correct classes
  expect_true(inherits(f, "linear_frequency"))
  expect_true(inherits(f, "property"))
  expect_equal(f$class_name, 'linear_frequency')
  expect_equal(f$dimension, Dimension$temporal)
  expect_equal(f$rotation,  Rotation$linear)
  expect_equal(f$measure,   Measure$rate)

  # Check if the value is set correctly (from the `property` class)
  expect_equal(f$value, 10)

  # Check that the default unit, symbol, and name are inherited from `property`
  expect_equal(f$unit, "Hz")
  expect_equal(f$unit_latex, "Hz")
  expect_equal(f$symbol, "f")
  expect_equal(f$symbol_latex, "f")
  expect_equal(f$name, "linear frequency")
})

test_that("linear_period subclass has correct classes", {
  # Create a linear_period object
  P <- linear_period(10)

  # Check that the object has the correct classes
  expect_true(inherits(P, "linear_period"))
  expect_true(inherits(P, "property"))
  expect_equal(P$class_name, 'linear_period')
  expect_equal(P$dimension, SPACE_TIME$temporal)
  expect_equal(P$rotation,  LINEAR_ANGULAR$linear)
  expect_equal(P$measure,   RATE_EXTENT$extent)

  # Check the value is set correctly (Prom the `property` class)
  expect_equal(P$value, 10)

  # Check that the unit, symbol, and name are inherited Prom `property`
  expect_equal(P$unit, "s")
  expect_equal(P$unit_latex, "\\text{s}")

  expect_equal(P$symbol, "T")
  expect_equal(P$symbol_latex, "T")

  expect_equal(P$name, "linear period")
})

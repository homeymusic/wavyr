test_that("angular_wavenumber subclass has correct classes", {
  # Create a angular_wavenumber object
  w <- angular_wavenumber(10)

  # Check that the object has the correct classes
  expect_true(inherits(w, "angular_wavenumber"))
  expect_true(inherits(w, "property"))
  expect_equal(w$class_name, 'angular_wavenumber')
  expect_equal(w$dimension, SPACE_TIME$spatial)
  expect_equal(w$rotation,  LINEAR_ANGULAR$angular)
  expect_equal(w$measure,   RATE_EXTENT$rate)

  # Check if the value is set correctly (from the `property` class)
  expect_equal(w$value, 10)

  # Check that the default unit, symbol, and name are inherited from `property`
  expect_equal(w$unit, "rad/m")
  expect_equal(w$unit_latex, "\\frac{\\text{rad}}{\\text{m}}")

  expect_equal(w$symbol, "k_angular")
  expect_equal(w$symbol_latex, "k_\\text{angular}")

  expect_equal(w$name, "angular wavenumber")
})

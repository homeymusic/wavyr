test_that("linear_wavenumber subclass has correct classes", {
  # Create a linear_wavenumber object
  l <- linear_wavenumber(10)

  # Check that the object has the correct classes
  expect_true(inherits(l, "linear_wavenumber"))
  expect_true(inherits(l, "property"))
  expect_equal(l$class_name, 'linear_wavenumber')
  expect_equal(l$space_time, SPACE_TIME$space)
  expect_equal(l$linear_angular,  LINEAR_ANGULAR$linear)
  expect_equal(l$rate_extent,   RATE_EXTENT$rate)

  # Check if the value is set correctly (from the `property` class)
  expect_equal(l$value, 10)

  expect_equal(l$unit, "1/m")
  expect_equal(l$unit_latex, "\\text{m}^{-1}")

  expect_equal(l$symbol, "k_linear")
  expect_equal(l$symbol_latex, "k_{\\text{linear}}")

  expect_equal(l$name, "linear wavenumber")
})

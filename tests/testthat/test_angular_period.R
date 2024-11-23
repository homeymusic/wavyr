test_that("angular_period subclass has correct classes", {
  # Create a angular_period object
  T_ang <- angular_period(10)

  # Check that the object has the correct classes
  expect_true(inherits(T_ang, "angular_period"))
  expect_true(inherits(T_ang, "property"))
  # Check if the value is set correctly (from the `property` class)
  expect_equal(T_ang$value, 10)
})
test_that("angular_period subclass has correct classes", {
  expect_equal(PROPERTIES$angular_period$name, 'angular period')
  expect_equal(PROPERTIES$angular_period$class_name, 'angular_period')
  expect_equal(PROPERTIES$angular_period$unit, "s/rad")
  expect_equal(PROPERTIES$angular_period$unit_latex, "\\frac{\\text{s}}{\\text{rad}}")
  expect_equal(PROPERTIES$angular_period$symbol, "T_angular")
  expect_equal(PROPERTIES$angular_period$symbol_latex, "\\Tau_\\text{angular}")
  expect_equal(PROPERTIES$angular_period$symbol_expression, 'italic(τ)["angular"]')
  expect_equal(PROPERTIES$angular_period$space_time, SPACE_TIME$time)
  expect_equal(PROPERTIES$angular_period$linear_angular,  LINEAR_ANGULAR$angular)
  expect_equal(PROPERTIES$angular_period$rate_extent,   RATE_EXTENT$extent)
})


test_that("angular_period subclass has correct classes", {
  # Create a angular_period object
  T_ang <- angular_period(10)

  # Check that the object has the correct classes
  expect_true(inherits(T_ang, "angular_period"))
  expect_true(inherits(T_ang, "property"))
  expect_equal(T_ang$class_name, 'angular_period')
  expect_equal(T_ang$dimension, SPACE_TIME$temporal)
  expect_equal(T_ang$rotation,  LINEAR_ANGULAR$angular)
  expect_equal(T_ang$measure,   RATE_EXTENT$extent)

  # Check if the value is set correctly (from the `property` class)
  expect_equal(T_ang$value, 10)

  # Check that the default unit, symbol, and name are inherited from `property`
  expect_equal(T_ang$unit, "s/rad")
  expect_equal(T_ang$unit_latex, "\\frac{\\text{s}}{\\text{rad}}")

  expect_equal(T_ang$symbol, "T_angular")
  expect_equal(T_ang$symbol_latex, "T_\\text{angular}")

  expect_equal(T_ang$name, "angular period")
})

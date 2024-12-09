test_that("linear_frequency subclass has correct classes", {
  f <- linear_frequency(10)
  expect_true(inherits(f, "linear_frequency"))
  expect_true(inherits(f, "property"))
  expect_equal(f$value, 10)
  expect_equal(f$name, "linear frequency")
  expect_equal(f$class_name, 'linear_frequency')
  expect_equal(f$unit, "Hz")
  expect_equal(f$unit_latex, "\\text{Hz}")
  expect_equal(f$unit_expression, "Hz")
  expect_equal(f$symbol, "f")
  expect_equal(f$symbol_latex, "f")
  expect_equal(f$symbol_expression, "italic(f)")
  expect_equal(f$space_time, SPACE_TIME$time)
  expect_equal(f$linear_angular,  LINEAR_ANGULAR$linear)
  expect_equal(f$extent_rate,   EXTENT_RATE$rate)
})

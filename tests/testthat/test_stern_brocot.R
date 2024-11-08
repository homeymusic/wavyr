# tests/testthat/test_stern_brocot.R

test_that("stern_brocot function returns correct rational approximation", {

  # Test case 1: Standard input with small uncertainty
  result <- stern_brocot(2.5, 0.01)
  expect_type(result, "double")
  expect_length(result, 2)

  # Check if the result is a reasonable approximation of 2.5
  approx_value <- result[1] / result[2]
  expect_true(abs(approx_value - 2.5) <= 0.01)

  # Test case 2: Edge case for small x, should return (1, very large denominator)
  result <- stern_brocot(0.001, 0.0001)
  expect_equal(result[1], 1)
  expect_true(result[2] > 900)

  # Test case 3: Large x value with moderate uncertainty
  result <- stern_brocot(100.75, 0.1)
  approx_value <- result[1] / result[2]
  expect_true(abs(approx_value - 100.75) <= 0.1)

  # Test case 4: Small uncertainty should result in very accurate fraction
  result <- stern_brocot(3.333, 0.001)
  approx_value <- result[1] / result[2]
  expect_true(abs(approx_value - 3.333) <= 0.001)

  # Test case 5: Invalid input (negative x)
  expect_error(stern_brocot(-2.5, 0.01), "STOP: x must be greater than 0")

  # Test case 6: Invalid input (negative uncertainty)
  expect_error(stern_brocot(2.5, -0.01), "STOP: uncertainty must be greater than 0")

  # Test case 7: Result should be an integer if x is an integer
  result <- stern_brocot(3, 0.1)
  expect_equal(result[1], 3)
  expect_equal(result[2], 1)
})

# tests/testthat/test_stern_brocot_cpp.R

test_that("stern_brocot_cpp function returns correct rational approximation", {
  # Test case 1: Standard input with small uncertainty
  result <- stern_brocot_cpp(2.5, 0.01)
  expect_s3_class(result, "data.frame")  # Expect a data frame
  expect_named(result, c("original_value", "num", "den", "uncertainty"))  # Check column names

  # Check if the approximation is reasonable
  approx_value <- result$num / result$den
  expect_true(abs(approx_value - 2.5) <= result$uncertainty)

  # Test case 2: Edge case for small x
  result <- stern_brocot_cpp(0.001, 0.0001)
  expect_equal(result$num, 1)
  expect_true(result$den > 900)
  expect_equal(result$original_value, 0.001)

  # Test case 3: Large x value with moderate uncertainty
  result <- stern_brocot_cpp(100.75, 0.1)
  approx_value <- result$num / result$den
  expect_true(abs(approx_value - 100.75) <= result$uncertainty)

  # Test case 4: Small uncertainty should result in very accurate fraction
  result <- stern_brocot_cpp(3.333, 0.001)
  approx_value <- result$num / result$den
  expect_true(abs(approx_value - 3.333) <= result$uncertainty)

  # Test case 5: Invalid input (negative x)
  expect_error(stern_brocot_cpp(-2.5, 0.01), "STOP: x must be greater than 0")

  # Test case 6: Invalid input (negative uncertainty)
  expect_error(stern_brocot_cpp(2.5, -0.01), "STOP: uncertainty must be greater than 0")

  # Test case 7: Result should be an integer if x is an integer
  result <- stern_brocot_cpp(3, 0.1)
  expect_equal(result$num, 3)
  expect_equal(result$den, 1)
  expect_equal(result$original_value, 3)
})

test_that("stern_brocot_cpp does not return zero numerator or denominator", {
  x <- 0.1666667
  uncertainty <- 3.0

  result <- stern_brocot_cpp(x, uncertainty)

  expect_true(result$num != 0, info = "Stern-Brocot should never return a 0 numerator")
  expect_true(result$den != 0, info = "Stern-Brocot should never return a 0 denominator")
  expect_equal(result$original_value, x, info = "The original value should match the input")
})

# tests/testthat/test_stern_brocot_cpp.R

expected_columns <- c(
  "original_value",
  "num",
  "den",
  "uncertainty",
  "depth",
  "path",
  "path_id", # Path as an integer identifier
  "shannon_entropy",
  "hamming_weight",
  "run_length_encoding"
)

test_that("if x is less than unceratinty it returns interesting stuff", {

  result <- stern_brocot_cpp(1, 2)  # Replace with actual test case if needed
  expect_equal(names(result), expected_columns)
  expect_equal(result$num, 1)  # Replace with actual expected value
  expect_equal(result$den, 1)  # Replace with actual expected value

  result <- stern_brocot_cpp(50.234, 60)  # Replace with actual test case if needed
  expect_equal(names(result), expected_columns)
  expect_equal(result$num, 50)  # Replace with actual expected value
  expect_equal(result$den, 1)  # Replace with actual expected value

  result <- stern_brocot_cpp(0.25, 0.5)  # Replace with actual test case if needed
  expect_equal(names(result), expected_columns)
  expect_equal(result$num, 1)  # Replace with actual expected value
  expect_equal(result$den, 2)  # Replace with actual expected value

  result <- stern_brocot_cpp(0.25, 0.5)  # Replace with actual test case if needed
  expect_equal(names(result), expected_columns)
  expect_equal(result$num, 1)  # Replace with actual expected value
  expect_equal(result$den, 2)  # Replace with actual expected value

})

test_that("stern_brocot_cpp function returns correct rational approximation", {
  # Test case 1: Standard input with small uncertainty
  result <- stern_brocot_cpp(2.5, 0.01)
  expect_s3_class(result, "data.frame")  # Expect a data frame
  expect_equal(names(result), expected_columns)

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

gabor_uncertainty = 1 / (4 * pi)

tritone_ratio     = midi_to_freq(66) / midi_to_freq(60)
tritone_result    = stern_brocot_cpp(tritone_ratio, gabor_uncertainty)  # Replace with actual test case if needed

P8_ratio          = midi_to_freq(72) / midi_to_freq(60)
P8_result         = stern_brocot_cpp(P8_ratio, gabor_uncertainty)  # Replace with actual test case if needed

test_that("depth_cpp computes correct values", {
  expect_equal(tritone_result$depth, 3)  # Replace with actual expected value
  expect_equal(P8_result$depth, 0)  # Replace with actual expected value
})

test_that("path_cpp computes correct values", {
  expect_equal(tritone_result$path, "010")  # Replace with actual expected value
  expect_equal(P8_result$path, "")  # Replace with actual expected value
})

test_that("path_id_cpp computes correct values", {
  expect_equal(tritone_result$path_id, 2)  # Replace with actual expected value
  expect_equal(P8_result$path_id, 0)  # Replace with actual expected value
})

test_that("shannon_entropy_cpp computes correct values", {
  expect_equal(tritone_result$shannon_entropy, 0.9182958,
               tolerance=0.1)  # Replace with actual expected value
  expect_equal(P8_result$shannon_entropy, 0,
               tolerance=0.1)  # Replace with actual expected value
})

test_that("hamming_weight_cpp computes correct values", {
  expect_equal(tritone_result$hamming_weight, 1)  # Replace with actual expected value
  expect_equal(P8_result$hamming_weight, 0)  # Replace with actual expected value
})

test_that("run_length_encoding_cpp computes correct values", {
  expect_equal(tritone_result$run_length_encoding, 3)  # Replace with actual expected value
  expect_equal(P8_result$run_length_encoding, 0)  # Replace with actual expected value
})

# tests/testthat/test_approximate_rational_fractions_cpp.R

test_that("approximate_rational_fractions_cpp works for simple inputs", {
  x <- c(1.5, 2.25, 3.33)
  uncertainty <- 1e-3
  deviation <- 1e-2

  result <- approximate_rational_fractions_cpp(x, uncertainty, deviation)

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c("rational_number", "pseudo_rational_number",
                    "pseudo_octave", "num", "den",
                    "approximation", "error", "uncertainty") %in% names(result)))

  # Check that input is preserved
  expect_equal(result$rational_number, x)

  # Check that errors are within uncertainty
  expect_true(all(abs(result$error) <= uncertainty))

  # Check that approximations are ratios of integers
  expect_equal(result$num / result$den, result$approximation)
})

test_that("approximate_rational_fractions_cpp handles edge cases", {
  x <- c(1, 2, 3, 0.5, 1e-6)
  uncertainty <- 1e-5
  deviation <- 1e-3

  result <- approximate_rational_fractions_cpp(x, uncertainty, deviation)

  # Check that very small values are approximated correctly
  expect_true(all(result$num > 0))
  expect_true(all(result$den > 0))
  expect_true(all(abs(result$error) <= uncertainty))
})

test_that("approximate_rational_fractions_cpp handles identical inputs", {
  x <- rep(2.5, 10)  # Repeated identical values
  uncertainty <- 1e-4
  deviation <- 1e-3

  result <- approximate_rational_fractions_cpp(x, uncertainty, deviation)

  # Expect only one unique approximation
  unique_approximations <- unique(result$approximation)
  expect_length(unique_approximations, 1)

  # Ensure approximation is close to the input
  expect_true(abs(unique_approximations - 2.5) <= uncertainty)
})

test_that("approximate_rational_fractions_cpp handles large numbers", {
  x <- c(1e3, 1e4, 1e5)
  uncertainty <- 1e-2
  deviation <- 1e-1

  result <- approximate_rational_fractions_cpp(x, uncertainty, deviation)

  # Check that approximations are close to inputs
  expect_true(all(abs(result$error) <= uncertainty))

  # Check that pseudo_octave is computed
  expect_true(is.numeric(result$pseudo_octave))
})

test_that("approximate_rational_fractions_cpp handles small uncertainties", {
  x <- c(1.12345, 2.67891)
  uncertainty <- 1e-6  # Very small uncertainty
  deviation <- 1e-2

  result <- approximate_rational_fractions_cpp(x, uncertainty, deviation)

  # Ensure that approximations are very close to inputs
  expect_true(all(abs(result$error) <= uncertainty))
})

test_that("approximate_rational_fractions_cpp handles edge cases with zero input", {
  x <- c(0, 1, 2)  # Include zero
  uncertainty <- 1e-3
  deviation <- 1e-2

  # Check for the error message thrown by the C++ function
  expect_error(
    approximate_rational_fractions_cpp(x, uncertainty, deviation),
    "x must be greater than 0"
  )
})

test_that("approximate_rational_fractions_cpp requires deviation > uncertainty", {
  x <- c(1.5, 2.25, 3.33)
  uncertainty <- 1e-3
  deviation <- 1e-4  # Less than uncertainty

  # Expect an error if deviation is less than or equal to uncertainty
  expect_error(
    approximate_rational_fractions_cpp(x, uncertainty, deviation),
    "Deviation must be greater than uncertainty."
  )
})

test_that("approximate_rational_fractions_cpp handles large but valid deviations", {
  x <- c(1.5, 2.25, 3.33)
  uncertainty <- 1e-3
  deviation <- 1e2  # Large but valid deviation

  # Expect an error if deviation is less than or equal to uncertainty
  expect_error(
    approximate_rational_fractions_cpp(x, uncertainty, deviation),
    "Pseudo octave must be greater than 1. The deviation value is likely too large."
  )
})

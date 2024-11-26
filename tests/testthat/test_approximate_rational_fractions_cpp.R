# tests/testthat/test_approximate_rational_fractions_cpp.R

test_that("approximate_rational_fractions_cpp works for simple inputs", {
  x <- c(1.5, 2.25, 3.33)
  uncertainty <- 1e-3
  deviation <- 1e-2

  result <- approximate_rational_fractions_cpp(x, uncertainty, deviation)

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c("x", "pseudo_x",
                    "pseudo_octave", "num", "den",
                    "rationalized_x", "error", "uncertainty") %in% names(result)))

  # Check that input is preserved
  expect_equal(result$x, x)

  # Check that errors are within uncertainty
  expect_true(all(abs(result$error) <= uncertainty))

  # Check that approximations are ratios of integers
  expect_equal(result$num / result$den, result$rationalized_x)
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
  unique_approximations <- unique(result$rationalized_x)
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

###
# metadata
###

test_that("approximate_rational_fractions_cpp handles metadata round-tripping", {
  x <- c(1.5, 2.25, 3.33)
  uncertainty <- 1e-3
  deviation <- 1e-2

  # Metadata to round-trip
  metadata <- data.frame(
    id = c("a", "b", "c"),
    notes = c("first", "second", "third")
  )

  result <- approximate_rational_fractions_cpp(x, uncertainty, deviation, metadata = metadata)

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c("x", "pseudo_x", "pseudo_octave", "num", "den",
                    "rationalized_x", "error", "uncertainty", "id", "notes") %in% names(result)))

  # Check that metadata is preserved
  expect_equal(result$id, metadata$id)
  expect_equal(result$notes, metadata$notes)

  # Check that input is preserved
  expect_equal(result$x, x)

  # Check that errors are within uncertainty
  expect_true(all(abs(result$error) <= uncertainty))
})

test_that("approximate_rational_fractions_cpp handles missing metadata gracefully", {
  x <- c(1.5, 2.25, 3.33)
  uncertainty <- 1e-3
  deviation <- 1e-2

  # Call without metadata
  result <- approximate_rational_fractions_cpp(x, uncertainty, deviation)

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_false(any(c("id", "notes") %in% names(result)))

  # Check that input is preserved
  expect_equal(result$x, x)

  # Check that errors are within uncertainty
  expect_true(all(abs(result$error) <= uncertainty))
})

test_that("approximate_rational_fractions_cpp adds new columns to metadata", {
  x <- c(1.5, 2.25, 3.33)
  uncertainty <- 1e-3
  deviation <- 1e-2

  # Metadata with existing and new columns
  metadata <- data.frame(
    id = c("a", "b", "c"),
    notes = c("first", "second", "third"),
    additional = c(10, 20, 30)
  )

  result <- approximate_rational_fractions_cpp(x, uncertainty, deviation, metadata = metadata)

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c("id", "notes", "additional") %in% names(result)))

  # Check that metadata is preserved
  expect_equal(result$id, metadata$id)
  expect_equal(result$notes, metadata$notes)
  expect_equal(result$additional, metadata$additional)

  # Check that input is preserved
  expect_equal(result$x, x)

  # Check that errors are within uncertainty
  expect_true(all(abs(result$error) <= uncertainty))
})

test_that("approximate_rational_fractions_cpp handles mismatched metadata rows gracefully", {
  x <- c(1.5, 2.25, 3.33)
  uncertainty <- 1e-3
  deviation <- 1e-2

  # Metadata with a mismatched number of rows
  metadata <- data.frame(
    id = c("a", "b"),  # Only 2 rows, but x has 3 elements
    notes = c("first", "second")
  )

  # Expect an error due to mismatch
  expect_error(
    approximate_rational_fractions_cpp(x, uncertainty, deviation, metadata = metadata),
    "Metadata must have the same number of rows as the input vector x."
  )
})

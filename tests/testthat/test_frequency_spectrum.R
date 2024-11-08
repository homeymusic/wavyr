# tests/testthat/test_frequency_spectrum.R

test_that("we can create a new frequency spectrum with frequencies and amplitudes", {
  # Create a frequency_spectrum object
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Expectations to check frequency_spectrum creation
  expect_s3_class(frequency_spectrum_obj, "frequency_spectrum")
  expect_s3_class(frequency_spectrum_obj, "spectrum")
  expect_equal(frequency_spectrum_obj$component, c(100, 200, 300))
  expect_equal(frequency_spectrum_obj$amplitude, c(1.0, 0.8, 0.5))
})

test_that("frequency_spectrum can calculate fundamental_cycle_length", {
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Test fundamental_cycle_length
  expect_true(is.numeric(frequency_spectrum_obj$fundamental_cycle_length()))
})

test_that("frequency_spectrum can calculate fractions", {
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Test fractions output
  fractions <- frequency_spectrum_obj$fractions()
  expect_equal(fractions$num, c(2, 3, 1))  # Replace with expected values if different
  expect_equal(fractions$den, c(1, 1, 1))  # Replace with expected values if different
})

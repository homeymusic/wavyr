# tests/testthat/test_frequency_spectrum.R

test_that("we can create a new frequency spectrum with multiple frequencies and amplitudes", {

  # Create a frequency_spectrum object
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Expectations to check frequency_spectrum creation
  expect_s3_class(frequency_spectrum_obj, "frequency_spectrum")
  expect_equal(length(frequency_spectrum_obj$frequency), 3)
  expect_equal(frequency_spectrum_obj$frequency, c(100, 200, 300))
  expect_equal(frequency_spectrum_obj$amplitude, c(1.0, 0.8, 0.5))
})

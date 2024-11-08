# tests/testthat/test_frequency_spectrum.R

test_that("frequency_spectrum object can be created with multiple frequencies and amplitudes", {
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  expect_s3_class(frequency_spectrum_obj, "frequency_spectrum")
  expect_equal(frequency_spectrum_obj$component, c(100, 200, 300))
  expect_equal(frequency_spectrum_obj$amplitude, c(1.0, 0.8, 0.5))
})

test_that("frequency_spectrum can calculate fundamental_cycle_length", {
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  expect_equal(frequency_spectrum_obj$fundamental_cycle_length(), 1)
})

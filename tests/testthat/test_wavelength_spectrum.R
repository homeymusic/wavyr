# tests/testthat/test_wavelength_spectrum.R

test_that("we can create a new wavelength spectrum with separate wavelength and amplitude vectors", {
  # Create a wavelength_spectrum object with separate vectors
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(1, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Expectations to check wavelength_spectrum creation
  expect_s3_class(wavelength_spectrum_obj, "wavelength_spectrum")
  expect_s3_class(wavelength_spectrum_obj, "spectrum")
  expect_equal(wavelength_spectrum_obj$component %>% sort(), c(1, 0.5, 0.33) %>% sort())
  expect_equal(wavelength_spectrum_obj$amplitude %>% sort(), c(1.0, 0.8, 0.5) %>% sort())
})

test_that("we can create a new wavelength spectrum with a list containing wavelength and amplitude", {
  # Create a wavelength_spectrum object with a list input
  wavelength_spectrum_obj <- wavelength_spectrum(
    list(wavelength = c(1, 0.5, 0.33), amplitude = c(1.0, 0.8, 0.5))
  )

  # Expectations to check wavelength_spectrum creation
  expect_s3_class(wavelength_spectrum_obj, "wavelength_spectrum")
  expect_s3_class(wavelength_spectrum_obj, "spectrum")
  expect_equal(wavelength_spectrum_obj$component, c(1, 0.5, 0.33) %>% sort())
  expect_equal(wavelength_spectrum_obj$amplitude, c(1.0, 0.8, 0.5) %>% sort())
})

test_that("wavelength_spectrum handles mismatched input lengths gracefully", {
  expect_error(
    wavelength_spectrum(wavelength = c(1, 0.5), amplitude = c(1.0, 0.8, 0.5)),
    "must be the same length"
  )
})

test_that("wavelength_spectrum validates numeric input", {
  expect_error(
    wavelength_spectrum(wavelength = c("a", "b"), amplitude = c(1.0, 0.8)),
    "must be numeric"
  )
  expect_error(
    wavelength_spectrum(wavelength = c(1, 0.5), amplitude = c("x", "y")),
    "must be numeric"
  )
})

test_that("wavelength_spectrum calculates relative_cycle_length correctly", {
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(1, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Test relative_cycle_length
  expect_true(is.numeric(wavelength_spectrum_obj$relative_cycle_length))
  expect_gt(wavelength_spectrum_obj$relative_cycle_length, 0)
})

test_that("wavelength_spectrum calculates fractions accurately", {
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(1, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Test fractions output
  fractions <- wavelength_spectrum_obj$fractions
  expect_equal(fractions$num, c(3, 1, 3))
  expect_equal(fractions$den, c(1, 1, 2))
})

test_that("wavelength_spectrum relative_cycle_length for single component returns 1", {
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(1),
    amplitude = c(1.0)
  )

  # Expect the fundamental cycle length for a single component to be 1
  expect_equal(wavelength_spectrum_obj$relative_cycle_length, 1)
})

test_that("wavelength_spectrum edge cases: zero or negative wavelengths", {
  expect_error(
    wavelength_spectrum(wavelength = c(1, 0), amplitude = c(1.0, 0.5)),
    "All component values must be positive."
  )
  expect_error(
    wavelength_spectrum(wavelength = c(-1, 0.5), amplitude = c(1.0, 0.8)),
    "All component values must be positive."
  )
})

test_that("wavelength_spectrum can combine with another wavelength_spectrum within tolerance", {
  # Create two wavelength_spectrum objects
  wavelength_spectrum1 <- wavelength_spectrum(
    wavelength = c(2.0, 1.0, 0.67),
    amplitude = c(1.0, 0.8, 0.5)
  )
  wavelength_spectrum2 <- wavelength_spectrum(
    wavelength = c(2.0, 1.01, 0.67),  # Close values to test tolerance
    amplitude = c(0.5, 0.4, 0.3)
  )

  # Combine wavelength_spectrum1 and wavelength_spectrum2 with a tolerance of 0.05
  combined_wavelength_spectrum <- combine_spectra(
    wavelength_spectrum1,
    wavelength_spectrum2,
    tolerance = 0.05
  )

  # Expected combined wavelength and amplitude values
  expected_wavelengths <- c(2.0, 1.0, 0.67)
  expected_amplitudes <- c(1.5, 1.2, 0.8)

  # Test the combined wavelength_spectrum
  expect_s3_class(combined_wavelength_spectrum, "wavelength_spectrum")
  expect_equal(combined_wavelength_spectrum$component %>% sort(),
               expected_wavelengths %>% sort(),
               tolerance = 0.1)
  expect_equal(combined_wavelength_spectrum$amplitude %>% sort(),
               expected_amplitudes %>% sort(),
               tolerance = 0.1)
})

# tests/testthat/test_wavelength_spectrum.R

test_that("wavelength_spectrum has accessible wavelength field", {
  # Create a wavelength_spectrum object
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(2.0, 1.0, 0.67),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Check that `wavelength` field is accessible and correct
  expect_equal(wavelength_spectrum_obj$wavelength, c(2.0, 1.0, 0.67) %>% sort())
  expect_equal(wavelength_spectrum_obj$amplitude, c(1.0, 0.8, 0.5) %>% sort())
  expect_s3_class(wavelength_spectrum_obj, "wavelength_spectrum")
  expect_s3_class(wavelength_spectrum_obj, "spectrum")
})

test_that("wavelength_spectrum plot works as expected", {
  # Create a frequency_spectrum object
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = SPEED_OF_SOUND / c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Capture the plot with vdiffr
  vdiffr::expect_doppelganger("wavelength spectrum plot", function() {
    plot(wavelength_spectrum_obj)
  })
})

test_that("fundamental_wavelength is correctly calculated in wavelength_spectrum", {
  # Define wavelength components and amplitudes
  wavelength_components <- c(3.43, 1.715, 1.1433)  # Corresponding to frequencies 100, 200, 300 Hz in meters
  amplitudes <- c(1.0, 0.8, 0.5)

  # Create wavelength_spectrum object
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = wavelength_components,
    amplitude = amplitudes
  )

  # Calculate expected fundamental wavelength
  expected_fundamental_wavelength <- wavelength_spectrum_obj$relative_cycle_length / max(wavelength_components)

  # Test that fundamental_wavelength is correctly assigned
  expect_equal(wavelength_spectrum_obj$fundamental_wavelength, expected_fundamental_wavelength)
})
test_that("fundamental wavelength of tritone is longer than P1", {
  # Define wavelength components and amplitudes
  tt_wavelength_components <- SPEED_OF_SOUND / c(261.6256 , 369.9944 , 523.2511 , 739.9888 ,1046.5023)
  tt_amplitudes <- c(1.0000000 ,1.0000000 ,1.3395254, 0.8912509 ,0.8912509)

  # Create wavelength_spectrum object
  tt_wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = tt_wavelength_components,
    amplitude = tt_amplitudes
  )

  expect_equal(tt_wavelength_spectrum_obj$fundamental_wavelength, 1 / 0.07,
               tolerance = 0.1)

  # Define wavelength components and amplitudes
  P1_wavelength_components <- SPEED_OF_SOUND / c(261.6256, 523.2511, 1046.5023)
  P1_amplitudes <- c(1.0000000, 1.3395254, 0.8912509)

  # Create wavelength_spectrum object
  P1_wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = P1_wavelength_components,
    amplitude = P1_amplitudes
  )

  expect_equal(P1_wavelength_spectrum_obj$fundamental_wavelength, 1 / 1.31,
               tolerance = 0.1)


  expect_true(tt_wavelength_spectrum_obj$fundamental_wavelength > P1_wavelength_spectrum_obj$fundamental_wavelength)
})

test_that("wavelength_spectrum plot works with beat spectrum", {
  # Create the main wavelength spectrum
  main_wavelength_spectrum <- wavelength_spectrum(
    wavelength = c(1, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create a beat spectrum
  beat_wavelength_spectrum <- wavelength_spectrum(
    wavelength = c(1.2, 0.6, 0.4),
    amplitude = c(0.7, 0.5, 0.3)
  )

  # Capture the plot with vdiffr to verify visual output with beat spectrum overlay
  vdiffr::expect_doppelganger("wavelength spectrum with beat overlay", function() {
    plot.wavelength_spectrum(
      main_wavelength_spectrum,
      beat_wavelength_spectrum = beat_wavelength_spectrum,
      beat_wavelength_spectrum_color = colors_homey$beat
    )
  })
})

test_that("wavelength_spectrum plot raises error if beat spectrum provided without color", {
  # Create the main wavelength spectrum
  main_wavelength_spectrum <- wavelength_spectrum(
    wavelength = c(1, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create a beat spectrum
  beat_wavelength_spectrum <- wavelength_spectrum(
    wavelength = c(1.2, 0.6, 0.4),
    amplitude = c(0.7, 0.5, 0.3)
  )

  # Expect error if beat spectrum is provided without a specified color
  expect_error(
    plot.wavelength_spectrum(
      main_wavelength_spectrum,
      beat_wavelength_spectrum = beat_wavelength_spectrum,
      beat_wavelength_spectrum_color = NULL
    ),
    "overlay_spectrum_color must be specified if overlay_spectrum is provided"
  )
})

test_that("wavelength_spectrum plot shows primary spectrum under overlapping beat spectrum segment", {
  # Create the main wavelength spectrum with an overlapping wavelength component
  main_wavelength_spectrum <- wavelength_spectrum(
    wavelength = c(1.0, 0.5, 0.33),  # Main wavelengths
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create a beat spectrum with one overlapping wavelength
  beat_wavelength_spectrum <- wavelength_spectrum(
    wavelength = c(1.0, 0.6, 0.4),  # 1.0 overlaps with main spectrum
    amplitude = c(0.7, 0.5, 0.3)
  )

  # Capture the plot with vdiffr to visually confirm that the main spectrum remains visible under the beat spectrum overlay
  vdiffr::expect_doppelganger("wavelength spectrum with overlapping beat overlay", function() {
    plot.wavelength_spectrum(
      main_wavelength_spectrum,
      beat_wavelength_spectrum = beat_wavelength_spectrum,
      beat_wavelength_spectrum_color = colors_homey$beat
    )
  })
})

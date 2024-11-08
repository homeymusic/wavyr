# test_that("we can create a LinearWaveform with a frequency spectrum and speed of sound", {
#   # Create a frequency_spectrum object
#   frequency_spectrum_obj <- frequency_spectrum(
#     frequency = c(100, 200, 300),
#     amplitude = c(1.0, 0.8, 0.5)
#   )
#
#   # Create a LinearWaveform object
#   linear_waveform_obj <- linear_waveform(
#     frequency_spectrum = frequency_spectrum_obj,
#     speed_of_sound = 343
#   )
#
#   # Expectations to check LinearWaveform creation
#   expect_s3_class(linear_waveform_obj, "LinearWaveform")
#   expect_s3_class(linear_waveform_obj, "waveform")
#   expect_s3_class(linear_waveform_obj$frequency_spectrum, "frequency_spectrum")
#   expect_s3_class(linear_waveform_obj$wavelength_spectrum, "wavelength_spectrum")
# })
#
# test_that("LinearWaveform calculates correct wavelengths for given frequencies", {
#   frequency_spectrum_obj <- frequency_spectrum(
#     frequency = c(100, 200, 300),
#     amplitude = c(1.0, 0.8, 0.5)
#   )
#
#   # Create the LinearWaveform object
#   linear_waveform_obj <- linear_waveform(
#     frequency_spectrum = frequency_spectrum_obj,
#     speed_of_sound = 343
#   )
#
#   # Calculate expected wavelengths
#   expected_wavelengths <- 343 / c(100, 200, 300)
#
#   # Check if the first three components of the wavelength spectrum match expected values
#   expect_equal(
#     linear_waveform_obj$wavelength_spectrum$component[1:3],
#     expected_wavelengths
#   )
# })
#
# test_that("LinearWaveform includes beat_spectrum as a separate attribute", {
#   frequency_spectrum_obj <- frequency_spectrum(
#     frequency = c(100, 200, 300),
#     amplitude = c(1.0, 0.8, 0.5)
#   )
#
#   # Create the LinearWaveform object
#   linear_waveform_obj <- linear_waveform(
#     frequency_spectrum = frequency_spectrum_obj,
#     speed_of_sound = 343
#   )
#
#   # Extract expected beat wavelengths using the formula from compute_beats_cpp
#   wavelengths <- 343 / frequency_spectrum_obj$component
#   beat_wavelengths <- outer(wavelengths, wavelengths, function(x, y) (x * y) / abs(x - y)) %>%
#     .[lower.tri(.)]
#   beat_amplitudes <- outer(frequency_spectrum_obj$amplitude, frequency_spectrum_obj$amplitude, "+") %>%
#     .[lower.tri(.)]
#
#   # Verify that beat_spectrum contains the expected beat wavelengths and amplitudes
#   expect_equal(linear_waveform_obj$beat_spectrum$component, beat_wavelengths)
#   expect_equal(linear_waveform_obj$beat_spectrum$amplitude, beat_amplitudes)
# })
#
# test_that("LinearWaveform assigns correct classes and structure", {
#   frequency_spectrum_obj <- frequency_spectrum(
#     frequency = c(250, 500, 1000),
#     amplitude = c(1.0, 0.7, 0.4)
#   )
#
#   # Create the LinearWaveform object
#   linear_waveform_obj <- linear_waveform(frequency_spectrum = frequency_spectrum_obj)
#
#   # Check for expected classes
#   expect_s3_class(linear_waveform_obj, "LinearWaveform")
#   expect_s3_class(linear_waveform_obj, "waveform")
#   expect_s3_class(linear_waveform_obj$frequency_spectrum, "frequency_spectrum")
#   expect_s3_class(linear_waveform_obj$wavelength_spectrum, "wavelength_spectrum")
# })
#
# test_that("LinearWaveform includes base_wavelength_spectrum as a separate attribute", {
#   frequency_spectrum_obj <- frequency_spectrum(
#     frequency = c(150, 300, 450),
#     amplitude = c(1.0, 0.5, 0.3)
#   )
#
#   # Create the LinearWaveform object
#   linear_waveform_obj <- linear_waveform(
#     frequency_spectrum = frequency_spectrum_obj,
#     speed_of_sound = 343
#   )
#
#   # Verify that base_wavelength_spectrum is included and correctly structured
#   expect_s3_class(linear_waveform_obj$base_wavelength_spectrum, "wavelength_spectrum")
#   expect_equal(
#     linear_waveform_obj$base_wavelength_spectrum$component,
#     343 / frequency_spectrum_obj$component
#   )
#   expect_equal(
#     linear_waveform_obj$base_wavelength_spectrum$amplitude,
#     frequency_spectrum_obj$amplitude
#   )
# })
#
# test_that("LinearWaveform correctly computes combined spectra", {
#   frequency_spectrum_obj <- frequency_spectrum(
#     frequency = c(200, 400),
#     amplitude = c(0.8, 0.4)
#   )
#
#   # Create the LinearWaveform object
#   linear_waveform_obj <- linear_waveform(
#     frequency_spectrum = frequency_spectrum_obj,
#     speed_of_sound = 343
#   )
#
#   # Check if the combined wavelength spectrum correctly aggregates with the beat spectrum
#   combined_wavelength_spectrum <- linear_waveform_obj$wavelength_spectrum
#   expect_equal(length(combined_wavelength_spectrum$component),
#                length(linear_waveform_obj$base_wavelength_spectrum$component) +
#                  length(linear_waveform_obj$beat_spectrum$component))
# })
#
# test_that("LinearWaveform handles zero frequency gracefully", {
#   frequency_spectrum_obj <- frequency_spectrum(
#     frequency = c(0, 400),  # 0 Hz should be handled
#     amplitude = c(0.8, 0.4)
#   )
#
#   # Expect an error when creating the LinearWaveform object
#   expect_error(
#     linear_waveform(frequency_spectrum = frequency_spectrum_obj, speed_of_sound = 343),
#     "division by zero"
#   )
# })
#
# test_that("LinearWaveform validates amplitude correctly", {
#   frequency_spectrum_obj <- frequency_spectrum(
#     frequency = c(150, 300),
#     amplitude = c(-1, 0.5)  # Invalid amplitude
#   )
#
#   # Expect an error due to invalid amplitude values
#   expect_error(
#     linear_waveform(frequency_spectrum = frequency_spectrum_obj, speed_of_sound = 343),
#     "must be positive"
#   )
# })

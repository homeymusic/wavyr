test_that("veryifry what happen with 11 harmonics", {
  # Define the fundamental frequency and create harmonics excluding the fundamental
  fundamental_freq <- 100
  harmonics <- 1:11  # Harmonics from 2nd to 11th
  frequencies <- harmonics * fundamental_freq
  amplitudes <- 1 / harmonics  # Amplitudes decrease with each harmonic for realism

  frequency_spectrum_obj <- frequency_spectrum(
    frequency = frequencies,
    amplitude = amplitudes
  )

  # Create the linear_waveform object
  linear_waveform_obj <- linear_waveform(
    frequency_spectrum = frequency_spectrum_obj
  )

  # Confirm that the fundamental frequency (100 Hz) is not in linear_waveform_obj$frequency_spectrum
  expect_true(any(linear_waveform_obj$frequency_spectrum$frequency == fundamental_freq))

  # Calculate the fundamental wavelength
  fundamental_wavelength <- SPEED_OF_SOUND / fundamental_freq
  tolerance <- 1e-6  # Set a small tolerance for floating-point comparison

  # Confirm that the fundamental wavelength is present in the wavelength_spectrum within tolerance
  wavelength_spectrum <- linear_waveform_obj$wavelength_spectrum
  expect_true(any(abs(wavelength_spectrum$wavelength - fundamental_wavelength) < tolerance))

  sub_t = 'All Harmonics'
  t = paste(sub_t, '~ Frequency Spectrum')

  vdiffr::expect_doppelganger("all harmonics frequency spectrum", function() {
    plot(linear_waveform_obj$frequency_spectrum,)
  })

  t = paste(sub_t, '~ Wavelength Spectrum')
  vdiffr::expect_doppelganger("all harmonics wavelength spectrum", function() {
    plot(linear_waveform_obj$wavelength_spectrum)
  })

})

test_that("linear_waveform recovers missing fundamental in wavelength spectrum with summed amplitude", {
  # Define the fundamental frequency and create harmonics excluding the fundamental
  fundamental_freq <- 100
  harmonics <- seq(2, 11)  # Harmonics from 2nd to 11th
  frequencies <- harmonics * fundamental_freq
  amplitudes <- 1 / harmonics  # Amplitudes decrease with each harmonic for realism

  frequency_spectrum_obj <- frequency_spectrum(
    frequency = frequencies,
    amplitude = amplitudes
  )

  # Create the linear_waveform object
  linear_waveform_obj <- linear_waveform(
    frequency_spectrum = frequency_spectrum_obj
  )

  # Confirm that the fundamental frequency (100 Hz) is not in linear_waveform_obj$frequency_spectrum
  expect_false(any(linear_waveform_obj$frequency_spectrum$frequency == fundamental_freq))

  # Calculate the fundamental wavelength
  fundamental_wavelength <- SPEED_OF_SOUND / fundamental_freq
  tolerance <- 1e-6  # Set a small tolerance for floating-point comparison

  # Confirm that the fundamental wavelength is present in the wavelength_spectrum within tolerance
  wavelength_spectrum <- linear_waveform_obj$wavelength_spectrum
  expect_true(any(abs(wavelength_spectrum$wavelength - fundamental_wavelength) < tolerance))

  # Check that the amplitude for this fundamental wavelength is the sum of all frequency amplitudes
  total_amplitude <- sum(amplitudes)
  fundamental_amplitude <- wavelength_spectrum$amplitude[which(abs(wavelength_spectrum$wavelength - fundamental_wavelength) < tolerance)]
  expect_true(fundamental_amplitude > total_amplitude)

  t = 'Missing Fundamental ~ Frequency Spectrum'
  vdiffr::expect_doppelganger(t, function() {
    plot(linear_waveform_obj$frequency_spectrum, rectangles = c(fundamental_freq),
         title = t)
  })

  t = 'Missing Fundamental ~ Wavelength Spectrum'
  vdiffr::expect_doppelganger(t, function() {
    plot(linear_waveform_obj$base_wavelength_spectrum,
         rectangles = c(fundamental_wavelength),
         title = t,
         beat_wavelength_spectrum = linear_waveform_obj$beat_wavelength_spectrum,
         beat_wavelength_spectrum_color = colors_homey$beat)
  })

})

test_that("linear_waveform recovers missing fundamental in wavelength spectrum with summed amplitude", {
  # Define the fundamental frequency and create harmonics excluding the fundamental
  fundamental_freq <- 100
  harmonics <- seq(4, 11)[-c(3,6,7)]  # Harmonics from 2nd to 11th
  frequencies <- harmonics * fundamental_freq
  amplitudes <- 1 / harmonics  # Amplitudes decrease with each harmonic for realism

  frequency_spectrum_obj <- frequency_spectrum(
    frequency = frequencies,
    amplitude = amplitudes
  )

  # Create the linear_waveform object
  linear_waveform_obj <- linear_waveform(
    frequency_spectrum = frequency_spectrum_obj
  )

  # Confirm that the fundamental frequency (100 Hz) is not in linear_waveform_obj$frequency_spectrum
  expect_false(any(linear_waveform_obj$frequency_spectrum$frequency == fundamental_freq))
  # confirm that only the expected harmonics are in the freq spectrum:
  expect_equal(harmonics, c(4,5,7,8,11))
  expect_equal(harmonics, round(linear_waveform_obj$frequency_spectrum$frequency / fundamental_freq))

  # Calculate the fundamental wavelength
  fundamental_wavelength <- SPEED_OF_SOUND / fundamental_freq
  tolerance <- 1e-6  # Set a small tolerance for floating-point comparison

  # Confirm that the fundamental wavelength is present in the wavelength_spectrum within tolerance
  wavelength_spectrum <- linear_waveform_obj$wavelength_spectrum
  expect_true(any(abs(wavelength_spectrum$wavelength - fundamental_wavelength) < tolerance))

  # Check that the amplitude for this fundamental wavelength is the sum of all frequency amplitudes
  total_amplitude <- sum(amplitudes)
  fundamental_amplitude <- wavelength_spectrum$amplitude[which(abs(wavelength_spectrum$wavelength - fundamental_wavelength) < tolerance)]
  expect_true(fundamental_amplitude > 0.5)

  missing_freqs = setdiff(1:11, harmonics) * fundamental_freq

  sub_t = 'Missing 1, 2, 3, 6, 9, 10x Harmonics'
  t = paste(sub_t, '~ Frequency Spectrum')
  vdiffr::expect_doppelganger(t, function() {
    plot(linear_waveform_obj$frequency_spectrum, rectangles = missing_freqs,
         title=t)
  })

  t = paste(sub_t, '~ Wavelength Spectrum')
  vdiffr::expect_doppelganger(t, function() {
    plot(linear_waveform_obj$base_wavelength_spectrum,
         rectangles = SPEED_OF_SOUND / missing_freqs,
         title = t,
         beat_wavelength_spectrum = linear_waveform_obj$beat_wavelength_spectrum,
         beat_wavelength_spectrum_color = colors_homey$beat)
  })

})

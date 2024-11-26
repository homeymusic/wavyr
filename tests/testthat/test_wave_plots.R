test_that("feynman freqs without superposition", {
  label <- '4,5 Hz no superposition'
  # Create a frequency_spectrum object
  f = c(4,5)
  a = c(1,1)
  frequency_spectrum_obj <- frequency_spectrum(
    idealized_frequency = f,
    amplitude = a
  )

  # Create the wave object
  wave_obj <- wavyr::wave(
    frequency_spectrum = frequency_spectrum_obj,
    phase = 0
  )

  vdiffr::expect_doppelganger(label, function() plot(wave_obj, label = label))
})

test_that("feynman freqs with superposition", {
  label <- '4,5 Hz superposition'
  # Create a frequency_spectrum object
  f = c(4,5)
  a = c(1,1)
  frequency_spectrum_obj <- frequency_spectrum(
    idealized_frequency = f,
    amplitude = a
  )

  # Create the wave object
  wave_obj <- wavyr::superposed_wave(
    frequency_spectrum = frequency_spectrum_obj,
    phase = 0
  )

  vdiffr::expect_doppelganger(label, function() plot(wave_obj, label = label))
})

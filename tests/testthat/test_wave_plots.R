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


# TODO: compare the idealized versus rationalized version of this chord
# both as a visual plot and as an audio chord.
# in fact for the paper it would be cool to show it as a 2D grating.
# where each freq ratio becomes an orientation angle and a

test_that("irrational wave", {
  label <- 'irrational 1, sqrt(3), e, pi'
  # Create a frequency_spectrum object
  freqs = c(1, sqrt(3), exp(1), pi)
  amps = rep(1, length(freqs))
  frequency_spectrum_obj <- frequency_spectrum(
    idealized_frequency = freqs,
    amplitude = amps
  )

  # Create the wave object
  wave_obj <- wavyr::wave(
    frequency_spectrum = frequency_spectrum_obj,
    phase = 0
  )

  vdiffr::expect_doppelganger(label, function() plot(wave_obj, label = label))
})


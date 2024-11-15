test_that("detailed: wave plot generates correctly with time and space grid", {
  label <- 'C4 with 5 Harmonics'
  # Create a frequency_spectrum object
  f = c(4,5)
  a = c(1,1)
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = f,
    amplitude = a
  )

  # Create the wave object
  wave_obj <- wavyr::wave(
    frequency_spectrum = frequency_spectrum_obj,
    phase = 0
  )

  vdiffr::expect_doppelganger(label, function() plot(wave_obj, label = label))
})

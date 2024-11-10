intervals <- list(
  # consonant intervals
  Major3 = c(60, 64),
  minor6 = c(60, 68),
  Major6 = c(60, 69),
  minor3 = c(60, 63),
  # perfect intervals
  Perfect5 = c(60, 67),
  Perfect4 = c(60, 65),
  Perfect1 = c(60, 60),
  Perfect8 = c(60, 72),
  # dissonant intervals
  tritone = c(60, 66),
  Major7 = c(60, 71),
  minor2 = c(60, 61),
  Major2 = c(60, 62),
  minor7 = c(60, 70)
)
framed_intervals <- purrr::map(intervals, ~c(.x, 72))

# Define test cases for each interval using vdiffr::expect_doppelganger
test_that("waveform plot for each interval matches snapshot", {
  purrr::walk2(framed_intervals, names(framed_intervals), function(interval_midi, label) {

    f = hrep::sparse_fr_spectrum(interval_midi, num_harmonics=2)

    # Create frequency_spectrum and wavelength_spectrum
    f_spectrum <- frequency_spectrum(
      frequency = f$x,
      amplitude = f$y
    )

    l_spectrum <- wavelength_spectrum(
      wavelength = 343 / f$x,
      amplitude  = f$y
    )

    # Create a dummy waveform object with frequency_spectrum and wavelength_spectrum
    waveform <- waveform(
      frequency_spectrum = f_spectrum,
      wavelength_spectrum = l_spectrum
    )

    # Generate the plot
    plot <- plot(waveform, label = label)

    # Use vdiffr to verify the plot against the snapshot
    vdiffr::expect_doppelganger(paste(label), plot)
  })
})

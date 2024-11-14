source(testthat::test_path("helper.R"))
# Define test cases for each interval using vdiffr::expect_doppelganger
test_that("wave plot for each interval matches snapshot", {
  purrr::walk2(framed_intervals, names(framed_intervals), function(interval_midi, label) {

    f <- hrep::sparse_fr_spectrum(interval_midi, num_harmonics = 2)

    # Create frequency_spectrum and wavelength_spectrum
    f_spectrum <- frequency_spectrum(
      frequency = f$x,
      amplitude = f$y
    )

    l_spectrum <- wavelength_spectrum(
      wavelength = SPEED_OF_SOUND / f$x,
      amplitude  = f$y
    )

    # Create the wave object with frequency_spectrum and wavelength_spectrum
    wave <- wave(
      frequency_spectrum = f_spectrum,
      wavelength_spectrum = l_spectrum
    )

    label = paste('Framed', label)

    # Use vdiffr to capture the plot
    vdiffr::expect_doppelganger(
      label,
      function() plot(wave, label = label, resolution = 100)
    )
  })
})

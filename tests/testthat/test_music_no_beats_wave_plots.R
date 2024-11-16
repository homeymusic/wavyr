source(testthat::test_path("helper.R"))
# Define test cases for each interval using vdiffr::expect_doppelganger
test_that("wave plot for each interval matches snapshot", {
  purrr::walk2(framed_intervals, names(framed_intervals), function(interval_midi, label) {

    f_spectrum <- interval_midi %>% frequency_spectrum_for(num_harmonics = 2)

    # Create the wave object with frequency_spectrum and wavelength_spectrum
    wave <- wave(
      frequency_spectrum = f_spectrum
    )

    label = paste('Framed', label)

    # Use vdiffr to capture the plot
    vdiffr::expect_doppelganger(
      label,
      function() plot(wave, label = label)
    )
  })
})

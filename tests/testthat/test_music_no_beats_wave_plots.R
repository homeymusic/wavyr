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

# Define test cases for each interval using vdiffr::expect_doppelganger
test_that("M3 Time Signal", {

  M3_spectrum = framed_intervals$Major3 %>% frequency_spectrum_for(num_harmonics = 2)

  # Create the signal object from the spectrum
  signal_obj <- time_signal(M3_spectrum)

  expect_equal(signal_obj$plot_color, colors_homey$major)

  # Define label and coordinate range
  label <- "M3 Time Signal"

  # Use vdiffr to capture and test the plot output
  vdiffr::expect_doppelganger(label, function() plot_details.signal(signal_obj, resolution = 2000))

})

# Define test cases for each interval using vdiffr::expect_doppelganger
test_that("M3 Time Heisen Signal", {

  M3_spectrum = framed_intervals$Major3 %>% frequency_spectrum_for(num_harmonics = 2)

  # Create the signal object from the spectrum
  signal_obj <- time_heisen_signal(M3_spectrum)

  expect_equal(signal_obj$plot_color, colors_homey$major)

  # Define label and coordinate range
  label <- "M3 Time Signal"

  # Use vdiffr to capture and test the plot output
  vdiffr::expect_doppelganger(label, function() plot_details.signal(signal_obj, resolution = 2000))

})

# Define test cases for each interval using vdiffr::expect_doppelganger
test_that("m6 Time Signal", {

  m6_spectrum = framed_intervals$minor6 %>% frequency_spectrum_for(num_harmonics = 2)

  # Create the signal object from the spectrum
  signal_obj <- time_signal(m6_spectrum)

  expect_equal(signal_obj$plot_color, colors_homey$major)

  # Define label and coordinate range
  label <- "m6 Time Signal"

  # Use vdiffr to capture and test the plot output
  vdiffr::expect_doppelganger(label, function() plot_details.signal(signal_obj, resolution = 2000))

})

# tests/testthat/test_spectrum.R

test_that("we can create a new spectrum with separate component and amplitude vectors", {
  # Create a spectrum object with separate vectors
  spectrum_obj <- spectrum(
    idealized_component = c(1.0, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Expectations to check spectrum creation
  expect_s3_class(spectrum_obj, "spectrum")
  expect_equal(sort(spectrum_obj$idealized_component), sort(c(1, 0.5, 0.33)))
  expect_equal(sort(spectrum_obj$amplitude), sort(c(1.0, 0.8, 0.5)))
})

test_that("we can create a new spectrum with a list containing component and amplitude", {
  # Create a spectrum object with a list input
  spectrum_obj <- spectrum(
    list(c(1, 0.5, 0.33), c(1.0, 0.8, 0.5))
  )

  # Expectations to check spectrum creation
  expect_s3_class(spectrum_obj, "spectrum")
  expect_equal(sort(spectrum_obj$idealized_component), sort(c(1, 0.5, 0.33)))
  expect_equal(sort(spectrum_obj$amplitude), sort(c(1.0, 0.8, 0.5)))
})

test_that("only  a component can create spectrum", {
  expect_error(spectrum(frequency = c(500, 1000), amplitude = c(0.9, 0.6)))
})

test_that("spectrum can calculate rationalized cycle metrics", {
  spectrum_obj <- spectrum(
    idealized_component = c(1, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  expect_true(is.numeric(spectrum_obj$rationalized_cycles_per_reference))
  expect_true(is.numeric(spectrum_obj$rationalized_fundamental))
  expect_true(is.numeric(spectrum_obj$rationalized_extent))

})

test_that("spectrum fractions inlcudes original component, ref component and amplitude", {
  expected_component = c(1, 0.5, 0.33)
  expected_amplitude = c(1.0, 0.8, 0.5)

  spectrum_obj <- spectrum(
    idealized_component = expected_component,
    amplitude = expected_amplitude
  )

  # Test fractions output
  fractions <- spectrum_obj$fractions
  expect_equal(names(fractions), c("idealized_x","rationalized_x","pseudo_x","pseudo_octave",
                                   "num","den","error","uncertainty",
                                   "denominator_component","amplitude"))
  expect_equal(fractions$idealized_x, c(1.000000, 1.515152, 3.030303),
               tolerance=0.1)
  expect_equal(fractions$amplitude, c(0.5, 0.8, 1.0))
  expect_equal(fractions$denominator_component,
               rep(min(expected_component), length(expected_component)))

})

test_that('rationalized compmonent works as expected', {
  expected_component = midi_to_freq(c(60,64,67))
  expected_amplitude = c(1.0, 0.8, 0.5)

  spectrum_obj <- spectrum(
    idealized_component = expected_component,
    amplitude = expected_amplitude
  )

  # Test fractions output
  fractions <- spectrum_obj$fractions
  expect_equal(fractions$amplitude, c(1.0, 0.8, 0.5))

  expect_equal(spectrum_obj$rationalized_component,
               min(expected_component) * c(1/1, 4/3, 3/2),
               tolerance=0.1)

  expect_equal(spectrum_obj$rationalized_component,
               min(expected_component) * fractions$rationalized_x,
               tolerance=0.1)

})

test_that("spectrum can calculate fractions", {
  spectrum_obj <- spectrum(
    idealized_component = c(1, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Test fractions output
  fractions <- spectrum_obj$fractions
  expect_equal(sort(fractions$num), sort(c(3, 1, 3)))
  expect_equal(sort(fractions$den), sort(c(1, 1, 2)))
})

test_that("spectrum can combine with another spectrum within tolerance", {
  # Create two spectrum objects
  spectrum1 <- spectrum(
    idealized_component = c(1.0, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )
  spectrum2 <- spectrum(
    idealized_component = c(1.0, 0.5001, 0.33),  # Close values to test tolerance
    amplitude = c(0.5, 0.4, 0.3)
  )

  # Combine spectrum1 and spectrum2 with a tolerance of 0.001
  combined_spectrum <- combine_spectra(
    spectrum1,
    spectrum2,
    tolerance = 0.001
  )

  # Expected combined component and amplitude values
  expected_components <- c(1.0, 0.5, 0.33)
  expected_amplitudes <- c(1.5, 1.2, 0.8)

  # Test the combined spectrum
  expect_s3_class(combined_spectrum, "spectrum")
  expect_equal(sort(combined_spectrum$idealized_component), sort(expected_components), tolerance = 0.01)
  expect_equal(sort(combined_spectrum$amplitude), sort(expected_amplitudes), tolerance = 0.01)
})

test_that("cycle length per component", {
  # Create a spectrum object with separate vectors
  spectrum_obj <- spectrum(
    idealized_component = c(1.0, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Expectations to check spectrum creation
  expect_s3_class(spectrum_obj, "spectrum")
  expect_equal(sort(spectrum_obj$idealized_component), sort(c(1, 0.5, 0.33)))
  expect_equal(sort(spectrum_obj$amplitude), sort(c(1.0, 0.8, 0.5)))
  expect_equal(sort(spectrum_obj$fractions$den), sort(c(1, 1, 2)))
  expect_equal(spectrum_obj$rationalized_cycles_per_reference, 2)
})

test_that("spectrum reduces closely spaced components within tolerance to a single component", {
  # Define components that are within FLOATING_POINT_TOLERANCE of each other
  base_frequency <- 100
  close_components <- c(
    base_frequency,
    base_frequency + FLOATING_POINT_TOLERANCE / 2,
    base_frequency + FLOATING_POINT_TOLERANCE / 3
  )
  amplitudes <- c(1.0, 0.8, 0.5)

  # Create a spectrum object with the close components and amplitudes
  spectrum_obj <- spectrum(
    idealized_component = close_components,
    amplitude = amplitudes
  )

  # Expected values
  expected_component <- base_frequency  # Representative component within tolerance
  expected_amplitude <- sum(amplitudes)  # Sum of amplitudes

  # Check that the resulting spectrum has only one component
  expect_equal(length(spectrum_obj$idealized_component), 1)
  expect_equal(spectrum_obj$idealized_component, expected_component, tolerance = FLOATING_POINT_TOLERANCE)
  expect_equal(spectrum_obj$amplitude, expected_amplitude)
})

test_that("spectrum plot works without overlay spectrum", {
  # Create a spectrum object without an overlay
  spectrum_obj <- spectrum(
    idealized_component = c(1.0, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Capture the plot with vdiffr to check visual output
  vdiffr::expect_doppelganger("spectrum plot without overlay", function() {
    plot.spectrum(spectrum_obj, x_label = "Component", segment_color = "red")
  })
})

test_that("spectrum plot works with overlay spectrum", {
  # Create a main spectrum object
  main_spectrum <- spectrum(
    idealized_component = c(1.0, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create an overlay spectrum object
  overlay_spectrum <- spectrum(
    idealized_component = c(0.8, 0.4, 0.25),
    amplitude = c(0.6, 0.4, 0.3)
  )

  # Capture the plot with vdiffr to check visual output
  vdiffr::expect_doppelganger("spectrum with overlay", function() {
    plot.spectrum(
      main_spectrum,
      x_label = "Component",
      segment_color = "red",
      overlay_spectrum = overlay_spectrum,
      overlay_spectrum_color = "blue"
    )
  })
})

test_that("spectrum plot raises error without overlay color", {
  # Create a main spectrum object
  main_spectrum <- spectrum(
    idealized_component = c(1.0, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Create an overlay spectrum object
  overlay_spectrum <- spectrum(
    idealized_component = c(0.8, 0.4, 0.25),
    amplitude = c(0.6, 0.4, 0.3)
  )

  # Expect an error when overlay_spectrum is provided without overlay_spectrum_color
  expect_error(
    plot.spectrum(
      main_spectrum,
      x_label = "Component",
      segment_color = "red",
      overlay_spectrum = overlay_spectrum
    ),
    "overlay_spectrum_color must be specified if overlay_spectrum is provided"
  )
})

test_that("fundamental_component is calculated correctly without inversion", {
  # Create a spectrum object without inversion
  spectrum_obj <- spectrum(
    idealized_component = c(2, 4, 8),
    amplitude = c(1.0, 0.8, 0.5),
    extent_rate = EXTENT_RATE$rate
  )

  # Expected fundamental component: min(component) / rationalized_cycle_length
  expected_fundamental_component <- min(spectrum_obj$idealized_component) / spectrum_obj$rationalized_cycles_per_reference

  # Test fundamental_component calculation
  expect_equal(spectrum_obj$rationalized_fundamental, expected_fundamental_component)
})

test_that("fundamental_component is calculated correctly with inversion", {
  # Create a spectrum object with inversion
  spectrum_obj <- spectrum(
    idealized_component = c(2, 4, 8),
    amplitude = c(1.0, 0.8, 0.5),
    extent_rate = EXTENT_RATE$extent
  )

  # Expected fundamental component: rationalized_cycle_length / max(component)
  expected_fundamental_component <- spectrum_obj$rationalized_cycles_per_reference * max(spectrum_obj$idealized_component)

  # Test fundamental_component calculation
  expect_equal(spectrum_obj$rationalized_fundamental, expected_fundamental_component)
})

test_that("fundamental_cycle_length is calculated correctly for non-inverted spectrum", {
  # Create a non-inverted spectrum (e.g., frequency spectrum)
  spectrum_obj <- spectrum(
    idealized_component = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5),
    extent_rate = EXTENT_RATE$rate
  )

  # Expected cycle length for non-inverted spectrum: 1 / fundamental_component
  expected_cycle_length <- 1 / spectrum_obj$rationalized_fundamental

  # Test if fundamental_cycle_length matches the expected value
  expect_equal(spectrum_obj$rationalized_extent, expected_cycle_length, tolerance = 1e-6)
})

test_that("fundamental_cycle_length is calculated correctly for inverted spectrum", {
  # Create an inverted spectrum (e.g., wavelength spectrum)
  spectrum_obj <- spectrum(
    idealized_component = c(1.0, 0.5, 0.25),
    amplitude = c(1.0, 0.8, 0.5),
    extent_rate = EXTENT_RATE$extent
  )

  # Expected cycle length for inverted spectrum is fundamental_component itself
  expected_cycle_length <- spectrum_obj$rationalized_fundamental

  # Test if fundamental_cycle_length matches the expected value
  expect_equal(spectrum_obj$rationalized_extent, expected_cycle_length, tolerance = 1e-6)
})

test_that("fundamental_cycle_length handles small components correctly", {
  # Test with small component values for non-inverted spectrum
  spectrum_obj <- spectrum(
    idealized_component = c(0.1, 0.2, 0.3),
    amplitude = c(1.0, 0.8, 0.5),
    extent_rate = EXTENT_RATE$rate
  )

  # Expected cycle length for non-inverted spectrum
  expected_cycle_length <- 1 / spectrum_obj$rationalized_fundamental

  # Check calculation
  expect_equal(spectrum_obj$rationalized_extent, expected_cycle_length, tolerance = 1e-6)
})

test_that("fundamental_cycle_length handles large components correctly", {
  # Test with large component values for inverted spectrum
  spectrum_obj <- spectrum(
    idealized_component = c(1000, 2000, 3000),
    amplitude = c(1.0, 0.8, 0.5),
    extent_rate = EXTENT_RATE$extent
  )

  # Expected cycle length for inverted spectrum
  expected_cycle_length <- spectrum_obj$rationalized_cycles_per_reference

  # Check calculation
  expect_equal(spectrum_obj$rationalized_extent, expected_cycle_length * max(spectrum_obj$idealized_component), tolerance = 1e-6)
})

test_that("signal stores the spectrum components and amplitudes correctly", {
  # Create a spectrum object with known components and amplitudes
  f = c(100, 125, 150)
  a = c(1, 0.75, 0.5)
  spectrum_obj <- spectrum(
    idealized_component = f,
    amplitude = a
  )
  expect_equal(spectrum_obj$extent_rate, EXTENT_RATE$rate)
  expect_equal(spectrum_obj$rationalized_cycles_per_reference, 4)
  expect_equal(spectrum_obj$rationalized_fundamental, 25)
  expect_equal(spectrum_obj$rationalized_extent, 1/spectrum_obj$rationalized_fundamental)

})

test_that("reference is calculated correctly when NULL in the spectrum class", {
  # Create a spectrum object with extent_rate = EXTENT_RATE$space
  spectrum_obj <- spectrum(
    idealized_component = c(1.0, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5),
    extent_rate = EXTENT_RATE$rate
  )

  # Expect the calculated reference to be min(component)
  expected_reference = min(spectrum_obj$idealized_component)
  expect_equal(spectrum_obj$reference_component, expected_reference)
  expect_equal(spectrum_obj$rationalized_fundamental,
               expected_reference / spectrum_obj$rationalized_cycles_per_reference)

  # Create a spectrum object with extent_rate = EXTENT_RATE$extent
  spectrum_obj_inverted <- spectrum(
    idealized_component = c(1.0, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5),
    extent_rate = EXTENT_RATE$extent
  )

  # Expect the calculated reference to be max(component)
  expected_reference = max(spectrum_obj_inverted$idealized_component)
  expect_equal(spectrum_obj_inverted$reference_component, expected_reference)
  expect_equal(spectrum_obj_inverted$rationalized_fundamental,
               expected_reference * spectrum_obj_inverted$rationalized_cycles_per_reference)
})

test_that("reference can be explicitly set in the spectrum class", {
  expected_reference = 0.5
  # Explicitly set reference with extent_rate = EXTENT_RATE$extent
  spectrum_obj <- spectrum(
    idealized_component = c(1.0, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5),
    extent_rate = EXTENT_RATE$extent,
    reference = expected_reference
  )

  # Expect the explicitly set reference to be used
  expect_equal(spectrum_obj$reference_component, expected_reference)
  expect_equal(spectrum_obj$rationalized_fundamental,
               expected_reference * spectrum_obj$rationalized_cycles_per_reference)

  # Explicitly set reference with extent_rate = EXTENT_RATE$rate
  expected_reference = 0.33
  spectrum_obj_inverted <- spectrum(
    idealized_component = c(1.0, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5),
    extent_rate = EXTENT_RATE$rate,
    reference = expected_reference
  )

  # Expect the explicitly set reference to be used
  expect_equal(spectrum_obj_inverted$reference_component, expected_reference)
  expect_equal(spectrum_obj_inverted$rationalized_fundamental,
               expected_reference / spectrum_obj_inverted$rationalized_cycles_per_reference)

})


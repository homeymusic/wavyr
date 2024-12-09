test_that("we can create a new frequency spectrum with separate frequency and amplitude vectors", {
  # Create a frequency_spectrum object with separate vectors
  frequency_spectrum_obj <- frequency_spectrum(
    idealized_frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Expectations to check frequency_spectrum creation
  expect_s3_class(frequency_spectrum_obj, "frequency_spectrum")
  expect_s3_class(frequency_spectrum_obj, "spectrum")
  expect_equal(frequency_spectrum_obj$idealized_component, c(100, 200, 300))
  expect_equal(frequency_spectrum_obj$amplitude, c(1.0, 0.8, 0.5))
})

test_that("we can create a new frequency spectrum with a list containing frequency and amplitude", {
  # Create a frequency_spectrum object with a list input
  frequency_spectrum_obj <- frequency_spectrum(
    list(idealized_frequency = c(100, 200, 300), amplitude = c(1.0, 0.8, 0.5))
  )

  # Expectations to check frequency_spectrum creation
  expect_s3_class(frequency_spectrum_obj, "frequency_spectrum")
  expect_s3_class(frequency_spectrum_obj, "spectrum")
  expect_equal(frequency_spectrum_obj$idealized_component, c(100, 200, 300))
  expect_equal(frequency_spectrum_obj$amplitude, c(1.0, 0.8, 0.5))
})

test_that("frequency_spectrum handles different input sizes gracefully", {
  expect_error(
    frequency_spectrum(idealized_frequency = c(100, 200), amplitude = c(1.0, 0.8, 0.5)),
    "must be the same length"
  )
})

test_that("frequency_spectrum validates numeric input", {
  expect_error(
    frequency_spectrum(idealized_frequency = c("a", "b"), amplitude = c(1.0, 0.8)),
    "must be numeric"
  )
  expect_error(
    frequency_spectrum(idealized_frequency = c(100, 200), amplitude = c("x", "y")),
    "must be numeric"
  )
})

test_that("frequency_spectrum can calculate rationalized_cycles_per_reference", {
  frequency_spectrum_obj <- frequency_spectrum(
    idealized_frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Test rationalized_cycles_per_reference
  expect_true(is.numeric(frequency_spectrum_obj$rationalized_cycles_per_reference))
  expect_gt(frequency_spectrum_obj$rationalized_extent, 0)
})

test_that("frequency_spectrum calculates fractions accurately", {
  frequency_spectrum_obj <- frequency_spectrum(
    idealized_frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  expect_equal(frequency_spectrum_obj$fractions$num, c(1,2,3))
  expect_equal(frequency_spectrum_obj$fractions$den, c(1, 1, 1))
})

test_that("frequency_spectrum calculates fractions for simple ratios", {
  frequency_spectrum_obj <- frequency_spectrum(
    idealized_frequency = c(200, 100),
    amplitude = c(1.0, 0.5)
  )

  # Test fractions output for simple ratio
  fractions <- frequency_spectrum_obj$fractions
  expect_equal(fractions$num, c(1,2))
  expect_equal(fractions$den, c(1, 1))
})

test_that("frequency_spectrum rationalized_cycles_per_reference handles single component", {
  frequency_spectrum_obj <- frequency_spectrum(
    idealized_frequency = c(100),
    amplitude = c(1.0)
  )

  # Expect the fundamental cycle length for a single component to return 1
  expect_equal(frequency_spectrum_obj$rationalized_cycles_per_reference, 1)
})

test_that("frequency_spectrum edge cases: zero or negative frequencies", {
  expect_error(
    frequency_spectrum(idealized_frequency = c(100, 0), amplitude = c(1.0, 0.5)),
    "component values must be positive"
  )
  expect_error(
    frequency_spectrum(idealized_frequency = c(-100, 200), amplitude = c(1.0, 0.8)),
    "component values must be positive"
  )
})

test_that("frequency_spectrum can combine with another frequency_spectrum within tolerance", {
  # Create two frequency_spectrum objects
  frequency_spectrum1 <- frequency_spectrum(
    idealized_frequency = c(1000, 500, 333),
    amplitude = c(1.0, 0.8, 0.5)
  )
  frequency_spectrum2 <- frequency_spectrum(
    idealized_frequency = c(1000, 500.1, 333),  # Close values to test tolerance
    amplitude = c(0.5, 0.4, 0.3)
  )

  # Combine frequency_spectrum1 and frequency_spectrum2 with a tolerance of 0.1
  combined_frequency_spectrum <- combine_spectra(
    frequency_spectrum1,
    frequency_spectrum2,
    tolerance = 1
  )

  # Expected combined frequency and amplitude values
  expected_frequencies <- c(1000, 500, 333)
  expected_amplitudes <- c(1.5, 1.2, 0.8)

  # Test the combined frequency_spectrum
  expect_s3_class(combined_frequency_spectrum, "frequency_spectrum")
  expect_equal(combined_frequency_spectrum$idealized_component %>% sort(),
               expected_frequencies %>% sort(),
               tolerance = 0.1)
  expect_equal(combined_frequency_spectrum$amplitude %>% sort(),
               expected_amplitudes %>% sort(),
               tolerance = 0.1)
})
# tests/testthat/test_frequency_spectrum.R

test_that("frequency_spectrum has accessible frequency field", {
  # Create a frequency_spectrum object
  frequency_spectrum_obj <- frequency_spectrum(
    idealized_frequency = c(1000, 500, 333),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Check that `frequency` field is accessible and correct
  expect_equal(sort(frequency_spectrum_obj$idealized_frequency), sort(c(1000, 500, 333)))
  expect_equal(sort(frequency_spectrum_obj$amplitude), sort(c(1.0, 0.8, 0.5)))
  expect_s3_class(frequency_spectrum_obj, "frequency_spectrum")
  expect_s3_class(frequency_spectrum_obj, "spectrum")
})

test_that("frequency_spectrum plot works as expected", {
  # Create a frequency_spectrum object
  frequency_spectrum_obj <- frequency_spectrum(
    idealized_frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Check that the object has the expected class hierarchy
  expect_s3_class(frequency_spectrum_obj, "frequency_spectrum")
  expect_s3_class(frequency_spectrum_obj, "spectrum")

  # Check that `frequency` and `amplitude` fields are accessible and correct
  expect_equal(frequency_spectrum_obj$idealized_component, c(100, 200, 300))
  expect_equal(frequency_spectrum_obj$amplitude, c(1.0, 0.8, 0.5))

  # Confirm that component and amplitude are numeric vectors
  expect_type(frequency_spectrum_obj$idealized_component, "double")
  expect_type(frequency_spectrum_obj$amplitude, "double")

  # Capture the plot with vdiffr
  vdiffr::expect_doppelganger("frequency spectrum plot", function() {
    plot(frequency_spectrum_obj)
  })
})

test_that("rationalized_fundamental is correctly calculated in frequency_spectrum", {
  # Define frequency components and amplitudes
  freq_components <- c(100, 200, 300)
  amplitudes <- c(1.0, 0.8, 0.5)

  # Create frequency_spectrum object
  frequency_spectrum_obj <- frequency_spectrum(
    idealized_frequency = freq_components,
    amplitude = amplitudes
  )

  # Calculate expected fundamental frequency
  expected_rationalized_fundamental <- min(freq_components) * frequency_spectrum_obj$rationalized_cycles_per_reference

  # Test that rationalized_fundamental is correctly assigned
  expect_equal(frequency_spectrum_obj$rationalized_fundamental_frequency, expected_rationalized_fundamental)
})

test_that("rationalized_fundamental or tritone is lower than P1", {
  # Define frequency components and amplitudes
  tt_freq_components <- c(261.6256 , 369.9944 , 523.2511 , 739.9888 ,1046.5023)
  tt_amplitudes <- c(1.0000000 ,1.0000000 ,1.3395254, 0.8912509 ,0.8912509)

  # Create frequency_spectrum object
  tt_frequency_spectrum_obj <- frequency_spectrum(
    idealized_frequency = tt_freq_components,
    amplitude = tt_amplitudes
  )

  expect_equal(tt_frequency_spectrum_obj$rationalized_fundamental_frequency, 13.08,
               tolerance = 0.1)

  # Define frequency components and amplitudes
  P1_freq_components <- c(261.6256, 523.2511, 1046.5023)
  P1_amplitudes <- c(1.0000000, 1.3395254, 0.8912509)

  # Create frequency_spectrum object
  P1_frequency_spectrum_obj <- frequency_spectrum(
    idealized_frequency = P1_freq_components,
    amplitude = P1_amplitudes
  )

  expect_equal(P1_frequency_spectrum_obj$rationalized_fundamental_frequency, 261.63,
               tolerance = 0.1)

  expect_true(tt_frequency_spectrum_obj$rationalized_fundamental_frequency < P1_frequency_spectrum_obj$rationalized_fundamental_frequency)
})

test_that("cycle lengths vary correctly for JI major triad in frequency_spectrum", {
  # Define frequency components based on a JI major triad on 100 Hz
  freq_components <- c(100, 125, 150)  # Corresponding to ratios 1/1, 5/4, and 3/2
  amplitudes <- c(1.0, 0.8, 0.5)  # Arbitrary amplitude values

  # Create frequency_spectrum object
  frequency_spectrum_obj <- frequency_spectrum(
    idealized_frequency = freq_components,
    amplitude = amplitudes
  )

  expect_equal(frequency_spectrum_obj$extent_rate, EXTENT_RATE$rate)
  expect_equal(frequency_spectrum_obj$rationalized_cycles_per_reference, 4)
  expect_equal(frequency_spectrum_obj$rationalized_fundamental_frequency, 25)
  expect_equal(frequency_spectrum_obj$rationalized_extent, 1/frequency_spectrum_obj$rationalized_fundamental_frequency)

  # Define the expected cycle lengths based on the JI ratios (1, 4, and 2)
  expected_cycle_lengths <- c(1, 4, 2)

  # Check if the calculated cycle lengths match the expected values
  expect_equal(frequency_spectrum_obj$fractions$rationalized_x %>% sort(), c(1,1.25,1.5))
  expect_equal(frequency_spectrum_obj$fractions$den %>% sort(), c(1,2,4))
})

test_that("reference is calculated correctly when NULL in the frequency_spectrum class", {
  # Create a frequency_spectrum object
  spectrum_obj <- frequency_spectrum(
    idealized_frequency = c(1.0, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5)
  )

  expected_reference = min(spectrum_obj$idealized_frequency)
  expect_equal(spectrum_obj$reference_component, expected_reference)
  expect_equal(spectrum_obj$rationalized_fundamental_frequency,
               expected_reference / spectrum_obj$rationalized_cycles_per_reference)

})

test_that("reference can be explicitly set in the frequency_spectrum class", {
  expected_reference = 0.5
  spectrum_obj <- frequency_spectrum(
    idealized_frequency = c(1.0, 0.5, 0.33),
    amplitude = c(1.0, 0.8, 0.5),
    reference = expected_reference
  )

  # Expect the explicitly set reference to be used
  expect_equal(spectrum_obj$reference_component, expected_reference)
  expect_equal(spectrum_obj$rationalized_fundamental_frequency,
               expected_reference / spectrum_obj$rationalized_cycles_per_reference)

})

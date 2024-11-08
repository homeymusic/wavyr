# tests/testthat/test_frequency_spectrum.R

test_that("we can create a new frequency spectrum with separate frequency and amplitude vectors", {
  # Create a frequency_spectrum object with separate vectors
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Expectations to check frequency_spectrum creation
  expect_s3_class(frequency_spectrum_obj, "frequency_spectrum")
  expect_s3_class(frequency_spectrum_obj, "spectrum")
  expect_equal(frequency_spectrum_obj$component, c(100, 200, 300))
  expect_equal(frequency_spectrum_obj$amplitude, c(1.0, 0.8, 0.5))
})

test_that("we can create a new frequency spectrum with a list containing frequency and amplitude", {
  # Create a frequency_spectrum object with a list input
  frequency_spectrum_obj <- frequency_spectrum(
    list(frequency = c(100, 200, 300), amplitude = c(1.0, 0.8, 0.5))
  )

  # Expectations to check frequency_spectrum creation
  expect_s3_class(frequency_spectrum_obj, "frequency_spectrum")
  expect_s3_class(frequency_spectrum_obj, "spectrum")
  expect_equal(frequency_spectrum_obj$component, c(100, 200, 300))
  expect_equal(frequency_spectrum_obj$amplitude, c(1.0, 0.8, 0.5))
})

test_that("frequency_spectrum handles different input sizes gracefully", {
  expect_error(
    frequency_spectrum(frequency = c(100, 200), amplitude = c(1.0, 0.8, 0.5)),
    "must be the same length"
  )
})

test_that("frequency_spectrum validates numeric input", {
  expect_error(
    frequency_spectrum(frequency = c("a", "b"), amplitude = c(1.0, 0.8)),
    "must be numeric"
  )
  expect_error(
    frequency_spectrum(frequency = c(100, 200), amplitude = c("x", "y")),
    "must be numeric"
  )
})

test_that("frequency_spectrum can calculate fundamental_cycle_length", {
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Test fundamental_cycle_length
  expect_true(is.numeric(frequency_spectrum_obj$fundamental_cycle_length()))
  expect_gt(frequency_spectrum_obj$fundamental_cycle_length(), 0)
})

test_that("frequency_spectrum calculates fractions accurately", {
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Test fractions output
  fractions <- frequency_spectrum_obj$fractions()
  expect_equal(fractions$num, c(2, 3, 1))
  expect_equal(fractions$den, c(1, 1, 1))
})

test_that("frequency_spectrum calculates fractions for simple ratios", {
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(200, 100),
    amplitude = c(1.0, 0.5)
  )

  # Test fractions output for simple ratio
  fractions <- frequency_spectrum_obj$fractions()
  expect_equal(fractions$num, c(2, 1))
  expect_equal(fractions$den, c(1, 1))
})

test_that("frequency_spectrum fundamental_cycle_length handles single component", {
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100),
    amplitude = c(1.0)
  )

  # Expect the fundamental cycle length for a single component to return 1
  expect_equal(frequency_spectrum_obj$fundamental_cycle_length(), 1)
})

test_that("frequency_spectrum edge cases: zero or negative frequencies", {
  expect_error(
    frequency_spectrum(frequency = c(100, 0), amplitude = c(1.0, 0.5)),
    "component values must be positive"
  )
  expect_error(
    frequency_spectrum(frequency = c(-100, 200), amplitude = c(1.0, 0.8)),
    "component values must be positive"
  )
})

test_that("frequency_spectrum can combine with another frequency_spectrum within tolerance", {
  # Create two frequency_spectrum objects
  frequency_spectrum1 <- frequency_spectrum(
    frequency = c(1000, 500, 333),
    amplitude = c(1.0, 0.8, 0.5)
  )
  frequency_spectrum2 <- frequency_spectrum(
    frequency = c(1000, 500.1, 333),  # Close values to test tolerance
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
  expect_equal(combined_frequency_spectrum$component %>% sort(),
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
    frequency = c(1000, 500, 333),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Check that `frequency` field is accessible and correct
  expect_equal(frequency_spectrum_obj$frequency, c(1000, 500, 333))
  expect_equal(frequency_spectrum_obj$amplitude, c(1.0, 0.8, 0.5))
  expect_s3_class(frequency_spectrum_obj, "frequency_spectrum")
  expect_s3_class(frequency_spectrum_obj, "spectrum")
})

test_that("frequency_spectrum plot works as expected", {
  # Create a frequency_spectrum object
  frequency_spectrum_obj <- frequency_spectrum(
    frequency = c(100, 200, 300),
    amplitude = c(1.0, 0.8, 0.5)
  )

  # Capture the plot with vdiffr
  vdiffr::expect_doppelganger("frequency spectrum plot", function() {
    plot(frequency_spectrum_obj)
  })
})

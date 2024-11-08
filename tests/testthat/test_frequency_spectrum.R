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

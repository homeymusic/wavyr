expected_conversions <- list(
  linear_frequency   = 349.22820000,
  linear_wavelength  = 1.00000000,
  linear_wavenumber  = 6.28318531,
  linear_period      = 0.00286346,
  angular_frequency  = 2194.26549509,
  angular_wavelength = 6.28318531,
  angular_wavenumber = 6.28318531,
  angular_period     = 0.01799163
)

test_that("from f I can go to all 8 properties", {

  # from f to f
  f = linear_frequency(expected_conversions$linear_frequency)
  expect_true(inherits(f, "linear_frequency"))
  expect_equal(f$value, expected_conversions$linear_frequency,
               tolerance = 0.1)

  # from f to l
  l = linear_wavelength(f)
  expect_true(inherits(l, "linear_wavelength"))
  expect_equal(l$value, expected_conversions$linear_wavelength,
               tolerance = 0.1)

  # from f to k_l
  k_l = linear_wavenumber(f)
  expect_true(inherits(k_l, "linear_wavenumber"))
  expect_equal(k_l$value, expected_conversions$linear_wavenumber,
               tolerance = 0.1)

})

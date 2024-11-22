expected_linear_frequency   = 440
expected_linear_period      = 1 / expected_linear_frequency
expected_linear_wavenumber  = expected_linear_frequency / DEFAULT_SPEED_OF_MEDIUM
expected_linear_wavelength  = DEFAULT_SPEED_OF_MEDIUM / expected_linear_frequency
expected_angular_frequency  = 2 * pi * expected_linear_frequency
expected_angular_period     = 1 / expected_angular_frequency
expected_angular_wavenumber = expected_angular_frequency / DEFAULT_SPEED_OF_MEDIUM
expected_angular_wavelength = DEFAULT_SPEED_OF_MEDIUM / expected_angular_frequency

test_that("from f I can go to all 8 properties", {

  # from f to f
  f = linear_frequency(expected_linear_frequency)
  expect_true(inherits(f, "linear_frequency"))
  expect_equal(f$value, expected_linear_frequency,
               tolerance = 0.1)

  # from f to T (P)
  P = linear_period(f)
  expect_true(inherits(P, "linear_period"))
  expect_equal(P$value, expected_linear_period,
               tolerance = 0.1)

  # from f to k_l
  k_l = linear_wavenumber(f)
  expect_true(inherits(k_l, "linear_wavenumber"))
  expect_equal(k_l$value, expected_linear_wavenumber,
               tolerance = 0.1)

  # from f to l
  l = linear_wavelength(f)
  expect_true(inherits(l, "linear_wavelength"))
  expect_equal(l$value, expected_linear_wavelength,
               tolerance = 0.1)

  # from f to w
  w = angular_frequency(f)
  expect_true(inherits(w, "angular_frequency"))
  expect_equal(w$value, expected_angular_frequency,
               tolerance = 0.1)

  # from f to T_a
  T_a = angular_period(f)
  expect_true(inherits(T_a, "angular_period"))
  expect_equal(T_a$value, expected_angular_period,
               tolerance = 0.1)

  # from f to k_a
  k_a = angular_wavenumber(f)
  expect_true(inherits(k_a, "angular_wavenumber"))
  expect_equal(k_a$value, expected_angular_wavenumber,
               tolerance = 0.1)

  # from f to k_a
  l_a = angular_wavelength(f)
  expect_true(inherits(l_a, "angular_wavelength"))
  expect_equal(l_a$value, expected_angular_wavelength,
               tolerance = 0.1)

})

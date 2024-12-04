test_that("linear_wavenumber_2D_spectrum constructor works with FFT input", {

  image_filename = "MaDukesRightEye"
  image_file_path <- test_path("images", paste0(image_filename, ".png"))

  # Load and preprocess the image
  original_image <- imager::load.image(image_file_path)
  grayscale_image <- imager::grayscale(original_image)
  grayscale_matrix <- as.matrix(grayscale_image)

  # Compute FFT components
  fft_result <- fftwtools::fftw2d(grayscale_matrix)
  # Construct the S3 class object
  lw_spectrum <- linear_wavenumber_2D_spectrum(fft_result)

  # Check that the class is correct
  expect_s3_class(lw_spectrum, "linear_wavenumber_2D_spectrum")

  # Validate the object structure
  expect_true(is.list(lw_spectrum))
  expect_named(lw_spectrum, c("real", "imag", "dimensions"))

  # Ensure the real and imaginary parts are stored as sparse Matrices
  expect_true(inherits(lw_spectrum$real, "Matrix"))
  expect_true(inherits(lw_spectrum$imag, "Matrix"))
  expect_true(inherits(lw_spectrum$real, "sparseMatrix"))
  expect_true(inherits(lw_spectrum$imag, "sparseMatrix"))

  # Ensure the real and imaginary parts match the original input
  expect_equal(as.matrix(lw_spectrum$real), Re(fft_result))
  expect_equal(as.matrix(lw_spectrum$imag), Im(fft_result))

  # Ensure the dimensions are stored correctly
  expect_equal(lw_spectrum$dimensions, dim(fft_result))

})

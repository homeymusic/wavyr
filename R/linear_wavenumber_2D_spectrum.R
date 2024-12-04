#' Constructor for linear_wavenumber_2D_spectrum
#'
#' This function creates an S3 object representing a 2D Fourier Transform spectrum
#' with linear wavenumbers, storing the real and imaginary parts as sparse Matrices.
#'
#' @param spectrum A matrix containing the output of a 2D FFT.
#' @return An object of class 'linear_wavenumber_2D_spectrum'.
#' @export
linear_wavenumber_2D_spectrum <- function(spectrum) {
  if (!is.matrix(spectrum)) {
    stop("Input spectrum must be a matrix.")
  }

  if (!is.complex(spectrum)) {
    stop("Input spectrum must contain complex values.")
  }

  # Convert real and imaginary parts to sparse Matrices
  real_part <- Matrix::Matrix(Re(spectrum), sparse = TRUE)
  imag_part <- Matrix::Matrix(Im(spectrum), sparse = TRUE)

  # Create and return the S3 object
  structure(
    list(
      real = real_part,
      imag = imag_part,
      dimensions = dim(spectrum)
    ),
    class = "linear_wavenumber_2D_spectrum"
  )
}

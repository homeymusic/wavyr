#' Apply Gabor Filter
#'
#' Applies a Gabor filter to a grayscale image matrix.
#'
#' @param grayscale_matrix A matrix representation of the grayscale image.
#' @param orientation Orientation of the Gabor filter in radians.
#' @param f Frequency of the sinusoidal wave.
#' @param kernel_size Size of the Gabor kernel.
#' @return A filtered image as a `cimg` object.
apply_gabor_filter <- function(grayscale_matrix, orientation, f, kernel_size) {
  # Compute Gabor parameters
  params <- calculate_gabor_params()

  # Generate a single complex Gabor kernel
  complex_kernel <- create_gabor_kernel(kernel_size, params$gamma, params$eta, orientation, f, psi = 0)

  # Perform convolution with real and imaginary components
  response <- imager::convolve(imager::as.cimg(grayscale_matrix), imager::as.cimg(Re(complex_kernel))) +
    1i * imager::convolve(imager::as.cimg(grayscale_matrix), imager::as.cimg(Im(complex_kernel)))

  # Return the magnitude of the response as an image
  imager::as.cimg(Mod(response), dim = dim(grayscale_matrix))
}

#' Create Gabor Kernel
#'
#' Generates a Gabor kernel with specific parameters.
#'
#' @param kernel_size Size of the kernel (must be odd).
#' @param gamma Scale factor for the Gaussian envelope in x.
#' @param eta Scale factor for the Gaussian envelope in y.
#' @param orientation Orientation of the filter in radians.
#' @param f Frequency of the sinusoidal wave.
#' @param psi Phase offset of the sinusoidal wave.
#' @return A 2D matrix representing the Gabor kernel.
create_gabor_kernel <- function(kernel_size, gamma, eta, orientation, f, psi) {
  if (kernel_size %% 2 == 0) {
    stop("Kernel size must be odd.")
  }

  # Generate a grid of coordinates
  half_size <- (kernel_size - 1) / 2
  x <- seq(-half_size, half_size, length.out = kernel_size)
  y <- seq(-half_size, half_size, length.out = kernel_size)
  grid <- expand.grid(x = x, y = y)

  # Rotate coordinates based on orientation
  xr <- grid$x * cos(orientation) + grid$y * sin(orientation)
  yr <- -grid$x * sin(orientation) + grid$y * cos(orientation)

  # Scaling factor for the Gaussian envelope
  scaling_factor <- f^2 / (pi * gamma * eta)

  # Generate the Gabor kernel using the complex exponential
  gabor <- scaling_factor *
    exp(-((f^2 * xr^2) / (gamma^2) + (f^2 * yr^2) / (eta^2))) *
    exp(1i * 2 * pi * f * xr)  # Use 1i for the imaginary unit

  # Reshape the Gabor kernel into a matrix
  kernel <- matrix(gabor, nrow = kernel_size, ncol = kernel_size)
  return(kernel)
}

#' Calculate Gabor Parameters
#'
#' Computes the parameters for the Gabor kernel based on predefined constants
#' from the reference paper.
#'
#' @return A list containing:
#'         - `gamma`: Scale factor for the Gaussian envelope in x.
#'         - `eta`: Scale factor for the Gaussian envelope in y.
calculate_gabor_params <- function() {
  list(gamma = 0.8, eta = 0.8)
}

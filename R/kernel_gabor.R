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
kernel_gabor <- function(kernel_size, gamma, eta, orientation, f, psi) {
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

  # Generate the Gabor kernel using the complex exponential
  gabor <-
    exp(-((f^2 * xr^2) / (gamma^2) + (f^2 * yr^2) / (eta^2))) *
    exp(1i * 2 * pi * f * xr)  # Use 1i for the imaginary unit

  # Reshape the Gabor kernel into a matrix
  kernel <- matrix(gabor, nrow = kernel_size, ncol = kernel_size)
  return(kernel)
}

#' Create an Image Media Object
#'
#' The `image_media` function creates an S3 object representing an image media file.
#' This object stores the original image, its idealized spectrum (FFT), the inverse
#' FFT result (idealized signal), the idealized image after inverse FFT, and the
#' Gabor-filtered image.
#'
#' @param x A file path (character string) pointing to the image media file.
#'          The file must exist and be a valid format supported by `imager::load.image`.
#' @return An S3 object of class `"image_media"` containing:
#'         - `original_image`: The original media content.
#'         - `original_dimensions`: Dimensions of the original image (height, width, depth, channels).
#'         - `idealized_spectrum`: The FFT of the grayscale version of the image.
#'         - `idealized_dimensions`: Dimensions of the idealized image (after inverse FFT).
#'         - `idealized_signal`: The inverse FFT result of the idealized spectrum.
#'         - `idealized_image`: The reconstructed image after applying the inverse FFT.
#'         - `gabor_filtered_image`: The Gabor-filtered image.
#' @examples
#' # Example usage:
#' image_file <- "path/to/image.png"
#' image_media_obj <- image_media(image_file)
#' plot(image_media_obj$gabor_filtered_image)
#'
image_media <- function(x) {
  # Validate the input
  if (is.character(x) && file.exists(x)) {
    original_image <- imager::load.image(x)
    original_dimensions <- dim(original_image)

    # Grayscale and matrix format
    grayscale_image <- imager::grayscale(original_image)
    grayscale_matrix <- as.matrix(grayscale_image)

    # Idealized FFT components
    idealized_spectrum <- fftwtools::fftw2d(grayscale_matrix)
    idealized_signal <- fftwtools::fftw2d(idealized_spectrum, inverse = 1)
    idealized_dimensions <- original_dimensions
    idealized_dimensions[length(idealized_dimensions)] <- 1
    idealized_image <- imager::as.cimg(Re(idealized_signal), dim = idealized_dimensions)

    # Multi-orientation Gabor filtering
    orientations <- seq(0, pi, length.out = 4)  # 4 orientations: 0°, 45°, 90°, 135°
    combined_gabor_filtered_matrix <- apply_multi_orientation_gabor_filter(grayscale_matrix, orientations)
    gabor_filtered_image <- imager::as.cimg(combined_gabor_filtered_matrix, dim = dim(grayscale_image))
  } else {
    stop("Invalid input: Expected a valid file path to a media file.")
  }

  obj <- list(
    original_image       = original_image,
    original_dimensions  = original_dimensions,
    idealized_spectrum   = idealized_spectrum,
    idealized_dimensions = idealized_dimensions,
    idealized_signal     = idealized_signal,
    idealized_image      = idealized_image,
    gabor_filtered_image = gabor_filtered_image
  )

  class(obj) <- "image_media"
  return(obj)
}

#' Apply Multi-Orientation Gabor Filters
#'
#' This function applies Gabor filters at multiple orientations to a grayscale image matrix
#' and combines the results by averaging them.
#'
#' @param image_matrix A 2D matrix representing the grayscale image.
#' @param orientations A numeric vector of orientations (in radians).
#' @return A 2D matrix representing the combined Gabor-filtered image.
apply_multi_orientation_gabor_filter <- function(image_matrix, orientations) {
  # Apply Gabor filter for each orientation
  filtered_images <- lapply(orientations, function(theta) {
    apply_gabor_filter(image_matrix, theta = theta)
  })

  # Combine the results by averaging
  combined_filtered_image <- Reduce("+", filtered_images) / length(orientations)

  return(combined_filtered_image)
}

#' Apply a Gabor Filter
#'
#' This function applies a single Gabor filter with a given orientation to a grayscale image matrix.
#'
#' @param image_matrix A 2D matrix representing the grayscale image.
#' @param theta Orientation of the filter in radians.
#' @return A 2D matrix representing the Gabor-filtered image.
apply_gabor_filter <- function(image_matrix, theta) {
  # Define Gabor filter parameters
  kernel_size <- 31      # Kernel size (must be odd)
  sigma <- 4.0           # Standard deviation of the Gaussian envelope
  lambda <- 10.0         # Wavelength of the sinusoidal wave
  gamma <- 0.5           # Aspect ratio
  psi <- 0               # Phase offset

  # Create the Gabor kernel for the given orientation
  kernel <- create_gabor_kernel(kernel_size, sigma, theta, lambda, gamma, psi)

  # Perform convolution
  filtered_matrix <- imager::convolve(imager::as.cimg(image_matrix), imager::as.cimg(kernel))

  # Convert back to a matrix
  filtered_matrix <- as.matrix(filtered_matrix)

  return(filtered_matrix)
}

#' Create a Gabor Kernel
#'
#' This function generates a Gabor kernel with the specified parameters.
#'
#' @param kernel_size Size of the kernel (must be odd).
#' @param sigma Standard deviation of the Gaussian envelope.
#' @param theta Orientation of the filter in radians.
#' @param lambda Wavelength of the sinusoidal wave.
#' @param gamma Aspect ratio of the Gaussian envelope.
#' @param psi Phase offset of the sinusoidal wave.
#' @return A 2D matrix representing the Gabor kernel.
create_gabor_kernel <- function(kernel_size, sigma, theta, lambda, gamma, psi) {
  # Ensure kernel size is odd
  if (kernel_size %% 2 == 0) {
    stop("Kernel size must be odd.")
  }

  # Create a grid of x and y coordinates
  half_size <- (kernel_size - 1) / 2
  x <- seq(-half_size, half_size, length.out = kernel_size)
  y <- seq(-half_size, half_size, length.out = kernel_size)
  grid <- expand.grid(x = x, y = y)

  # Rotate the coordinates
  xr <- grid$x * cos(theta) + grid$y * sin(theta)
  yr <- -grid$x * sin(theta) + grid$y * cos(theta)

  # Compute the Gabor kernel
  gabor <- exp(-0.5 * ((xr^2 / sigma^2) + (gamma^2 * yr^2 / sigma^2))) *
    cos(2 * pi * xr / lambda + psi)

  # Reshape into a matrix
  kernel <- matrix(gabor, nrow = kernel_size, ncol = kernel_size)

  return(kernel)
}

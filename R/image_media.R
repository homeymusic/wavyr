#' Create an Image Media Object
#'
#' The `image_media` function creates an S3 object representing an image media file.
#' This object stores the original image, its idealized spectrum (FFT), the inverse
#' FFT result (idealized signal), the idealized image after inverse FFT, and methods
#' for Gabor filtering and spatial frequency mappings.
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
#'         - `idealized_spatial_frequencies`: A matrix mapping each cell to its spatial frequency values (x, y).
#'         - `rationalized_spatial_frequencies`: A matrix where each cell contains a rational approximation of y / x.
#'         - `gabor_filtered_image`: A method to generate Gabor-filtered images.
#' @examples
#' # Example usage:
#' image_file <- "path/to/image.png"
#' image_media_obj <- image_media(image_file)
#' plot(image_media_obj$idealized_image)
#'
image_media <- function(x) {
  # Validate input
  if (!(is.character(x) && file.exists(x))) {
    stop("Invalid input: Expected a valid file path to a media file.")
  }

  # Load and preprocess the image
  original_image <- imager::load.image(x)
  original_dimensions <- dim(original_image)
  grayscale_image <- imager::grayscale(original_image)
  grayscale_matrix <- as.matrix(grayscale_image)

  # Compute FFT components
  idealized_spectrum <- fftwtools::fftw2d(grayscale_matrix)
  idealized_dimensions <- original_dimensions
  idealized_dimensions[length(idealized_dimensions)] <- 1

  idealized_signal <- fftwtools::fftw2d(idealized_spectrum, inverse = 1)
  idealized_image <- imager::as.cimg(Re(idealized_signal), dim = idealized_dimensions)

  # Method for Gabor-filtered images
  gabor_filtered_image <- function(orientation, frequency = 0.1, kernel_size = 31) {
    gamma = 0.8
    eta   = 0.8
    phase = 0

    gabor_kernel <- kernel_gabor(kernel_size, gamma, eta, orientation, frequency, phase)
    convolved <- convolution_with_kernel(grayscale_matrix, gabor_kernel)
    filtered_image <- imager::as.cimg(Mod(convolved), dim = dim(grayscale_matrix))

  }

  # Create the S3 object
  obj <- list(
    original_image                   = original_image,
    original_dimensions              = original_dimensions,
    idealized_spectrum               = idealized_spectrum,
    idealized_dimensions             = idealized_dimensions,
    idealized_signal                 = idealized_signal,
    idealized_image                  = idealized_image,
    gabor_filtered_image             = gabor_filtered_image
  )

  # Assign S3 class
  class(obj) <- "image_media"
  return(obj)
}

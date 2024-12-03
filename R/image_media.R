#' Create an Image Media Object
#'
#' The `image_media` function creates an S3 object representing an image media file.
#' This object stores the original image, its idealized spectrum (FFT), the inverse
#' FFT result (idealized signal), the idealized image after inverse FFT, and a method
#' to generate Gabor-filtered images for specific orientations.
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
#'         - `spatial_frequencies`: A matrix mapping each cell to its spatial frequency values (kx, ky).
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

  # Create spatial frequency map
  spatial_frequencies <- spatial_frequency_map(nrow(grayscale_matrix), ncol(grayscale_matrix))

  # Method for Gabor-filtered images
  gabor_filtered_image <- function(orientation, f = 0.2, kernel_size = 31) {
    apply_gabor_filter(grayscale_matrix, orientation, f, kernel_size)
  }

  # Create the S3 object
  obj <- list(
    original_image       = original_image,
    original_dimensions  = original_dimensions,
    idealized_spectrum   = idealized_spectrum,
    idealized_dimensions = idealized_dimensions,
    idealized_signal     = idealized_signal,
    idealized_image      = idealized_image,
    spatial_frequencies  = spatial_frequencies,
    gabor_filtered_image = gabor_filtered_image
  )

  # Assign S3 class
  class(obj) <- "image_media"
  return(obj)
}

#' Spatial Frequency Map
#'
#' Creates a matrix where each cell contains a named vector of spatial frequencies (kx, ky).
#'
#' @param nrows Number of rows in the image.
#' @param ncols Number of columns in the image.
#' @return A matrix with each cell containing spatial frequency values as a named vector.
spatial_frequency_map <- function(nrows, ncols) {
  row_indices <- seq(0, nrows - 1)
  col_indices <- seq(0, ncols - 1)

  ky_values <- ifelse(row_indices > nrows / 2, row_indices - nrows, row_indices)
  kx_values <- ifelse(col_indices > ncols / 2, col_indices - ncols, col_indices)

  frequencies <- matrix(vector("list", nrows * ncols), nrow = nrows, ncol = ncols)
  for (i in seq_len(nrows)) {
    for (j in seq_len(ncols)) {
      frequencies[[i, j]] <- c(kx = kx_values[j], ky = ky_values[i])
    }
  }
  return(frequencies)
}

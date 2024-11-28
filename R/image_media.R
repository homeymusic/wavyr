#' Create an Image Media Object
#'
#' The `image_media` function creates an S3 object representing an image media file.
#' This object stores the original image, its idealized spectrum (FFT), the inverse
#' FFT result (idealized signal), and the idealized image after inverse FFT. It
#' also stores the dimensions of both the original and idealized images.
#'
#' @param x A file path (character string) pointing to the image media file.
#'          The file must exist and be a valid format supported by `imager::load.image`.
#' @return An S3 object of class `"image_media"` containing:
#'         - `original`: The original media content (e.g., an image matrix).
#'         - `original_dimensions`: Dimensions of the original image (height, width, depth, channels).
#'         - `idealized_spectrum`: The FFT of the grayscale version of the image.
#'         - `idealized_dimensions`: Dimensions of the idealized image (after inverse FFT).
#'         - `idealized_signal`: The inverse FFT result of the idealized spectrum.
#'         - `idealized_image`: The reconstructed image after applying the inverse FFT.
#' @examples
#' # Example usage:
#' image_file <- "path/to/image.png"
#' image_media_obj <- image_media(image_file)
#' print(image_media_obj)
#' plot(image_media_obj$idealized_image)
#'
image_media <- function(x) {
  # Validate the input: check if it is a file path
  if (is.character(x) && file.exists(x)) {
    original_image <- imager::load.image(x)
    original_dimensions <- dim(original_image)
    idealized_spectrum <- fftwtools::fftw2d(as.matrix(imager::grayscale(original_image)))
    idealized_dimensions <- original_dimensions
    idealized_dimensions[length(idealized_dimensions)] <- 1  # Set channels to 1 for grayscale image
    idealized_signal <- fftwtools::fftw2d(idealized_spectrum, inverse = 1)
    idealized_image <- imager::as.cimg(Re(idealized_signal),
                                       dim = idealized_dimensions)
  } else {
    # If the input is not a valid file path, raise an error
    stop("Invalid input: Expected a valid file path to a media file.")
  }

  # Create the image_media object (list) and populate it with the computed values
  obj <- list(
    original_image       = original_image,
    original_dimensions  = original_dimensions,
    idealized_spectrum   = idealized_spectrum,
    idealized_dimensions = idealized_dimensions,
    idealized_signal     = idealized_signal,
    idealized_image      = idealized_image
  )

  # Assign the S3 class to the object (makes it an "image_media" object)
  class(obj) <- "image_media"

  # Return the created object
  return(obj)
}

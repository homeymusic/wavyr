#' Create a Media Object
#'
#' The `media` function creates an S3 object representing a media file, such as an image,
#' by loading its content into memory. It validates the input and ensures the media
#' content is accessible for further processing.
#'
#' @param x A file path (character string) pointing to the media file.
#'          The file must exist and be a valid format supported by `imager::load.image`.
#' @return An S3 object of class `"media"` containing:
#'         - `original_signal`: The actual media content (e.g., an image matrix).
#' @examples
#' # Example usage:
#' image_file <- "path/to/image.webp"
#' media_obj <- media(image_file)
#' print(media_obj)
media <- function(x) {
  # Check if the input is a valid file path
  if (is.character(x) && file.exists(x)) {
    # Load the media content (e.g., image) from the specified file
    original_signal <- imager::load.image(x)
  } else {
    # Stop execution with an error message if the input is invalid
    stop("Invalid input: Expected a valid file path to a media file.")
  }

  # Create a list to store the media content
  obj <- list(
    # Store the loaded media content under the 'original_signal' attribute
    original_signal = original_signal
  )

  # Assign the S3 class "media" to the object
  class(obj) <- "media"

  # Return the media object
  return(obj)
}

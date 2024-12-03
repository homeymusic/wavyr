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

  # Create spatial frequency maps
  idealized_spatial_frequencies <- idealized_spatial_frequency_map(nrow(grayscale_matrix), ncol(grayscale_matrix))
  rationalized_spatial_frequencies <- rationalized_spatial_frequency_map(idealized_spatial_frequencies)

  # Create rationalized spectrum
  rationalized_spectrum <- create_rationalized_spectrum(idealized_spectrum, rationalized_spatial_frequencies)

  # Method for Gabor-filtered images
  gabor_filtered_image <- function(orientation, f = 0.2, kernel_size = 31) {
    apply_gabor_filter(grayscale_matrix, orientation, f, kernel_size)
  }

  # Create the S3 object
  obj <- list(
    original_image                   = original_image,
    original_dimensions              = original_dimensions,
    idealized_spectrum               = idealized_spectrum,
    idealized_dimensions             = idealized_dimensions,
    idealized_signal                 = idealized_signal,
    idealized_image                  = idealized_image,
    idealized_spatial_frequencies    = idealized_spatial_frequencies,
    rationalized_spatial_frequencies = rationalized_spatial_frequencies,
    rationalized_spectrum            = rationalized_spectrum,
    gabor_filtered_image             = gabor_filtered_image
  )

  # Assign S3 class
  class(obj) <- "image_media"
  return(obj)
}

#' Spatial Frequency Map
#'
#' Creates a matrix where each cell contains a named vector of spatial frequencies (x, y).
#'
#' @param nrows Number of rows in the image.
#' @param ncols Number of columns in the image.
#' @return A matrix with each cell containing spatial frequency values as a named vector.
idealized_spatial_frequency_map <- function(nrows, ncols) {
  row_indices <- seq(0, nrows - 1)
  col_indices <- seq(0, ncols - 1)

  y_values <- ifelse(row_indices > nrows / 2, row_indices - nrows, row_indices)
  x_values <- ifelse(col_indices > ncols / 2, col_indices - ncols, col_indices)

  spatial_frequencies <- matrix(vector("list", nrows * ncols), nrow = nrows, ncol = ncols)
  for (i in seq_len(nrows)) {
    for (j in seq_len(ncols)) {
      spatial_frequencies[[i, j]] <- c(x = x_values[j], y = y_values[i])
    }
  }
  return(spatial_frequencies)
}

#' Rationalized Spatial Frequency Map
#'
#' Creates a matrix where each cell contains a rationalized approximation of y / x using Stern-Brocot.
#'
#' @param spatial_frequencies A matrix with each cell containing spatial frequencies (x, y).
#' @return A matrix with rationalized spatial frequencies.
rationalized_spatial_frequency_map <- function(spatial_frequencies) {
  nrows <- nrow(spatial_frequencies)
  ncols <- ncol(spatial_frequencies)
  rationalized_frequencies <- matrix(vector("list", nrows * ncols), nrow = nrows, ncol = ncols)

  for (i in seq_len(nrows)) {
    for (j in seq_len(ncols)) {
      k <- spatial_frequencies[[i, j]]
      x <- k["x"]
      y <- k["y"]

      if (x == 0 || y == 0) {
        # Handle edge cases with zero values
        rationalized_frequencies[[i, j]] <- k
      } else {
        # Compute rationalized values using Stern-Brocot
        abs_ratio <- abs(y / x) %>% unname()
        sb_result <- stern_brocot_cpp(abs_ratio, uncertainty = GABOR_UNCERTAINTY)
        num <- sb_result$num
        den <- sb_result$den

        # Reapply signs
        rationalized_frequencies[[i, j]] <- c(sign(x) * den, sign(y) * num)
      }
    }
  }
  return(rationalized_frequencies)
}

#' Create Rationalized Spectrum
#'
#' Transforms the `idealized_spectrum` into the `rationalized_spectrum` by aggregating
#' complex values into cells defined by the `rationalized_spatial_frequencies`.
#'
#' @param idealized_spectrum The original 2D FFT spectrum (complex values).
#' @param rationalized_spatial_frequencies A matrix of rationalized spatial frequency mappings.
#' @return A matrix representing the rationalized spectrum.
create_rationalized_spectrum <- function(idealized_spectrum, rationalized_spatial_frequencies) {
  nrows <- nrow(idealized_spectrum)
  ncols <- ncol(idealized_spectrum)

  # Initialize the rationalized spectrum with the same dimensions as the original
  rationalized_spectrum <- matrix(0 + 0i, nrow = nrows, ncol = ncols)

  for (i in seq_len(nrows)) {
    for (j in seq_len(ncols)) {
      # Get the rationalized coordinates for this cell
      rationalized_coords <- rationalized_spatial_frequencies[[i, j]]
      x_prime <- rationalized_coords["x"]
      y_prime <- rationalized_coords["y"]

      # Map the rationalized coordinates back to matrix indices
      target_row <- (y_prime + nrows) %% nrows + 1  # Ensure cyclic indexing
      target_col <- (x_prime + ncols) %% ncols + 1

      # Aggregate the value from the idealized spectrum into the rationalized spectrum
      rationalized_spectrum[target_row, target_col] <-
        rationalized_spectrum[target_row, target_col] + idealized_spectrum[i, j]
    }
  }

  return(rationalized_spectrum)
}

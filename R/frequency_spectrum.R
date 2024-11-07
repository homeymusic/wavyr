#' Create a frequency spectrum
#'
#' @param frequency A numeric vector of frequencies
#' @param amplitude A numeric vector of amplitudes corresponding to each frequency
#'
#' @return An object of class "frequency_spectrum" containing the frequencies and amplitudes
#' @export
frequency_spectrum <- function(frequency, amplitude) {
  # Validate inputs
  if (length(frequency) != length(amplitude)) {
    stop("frequency and amplitude must be the same length")
  }

  # Create an S3 object with structure
  structure(
    list(frequency = frequency, amplitude = amplitude),
    class = "frequency_spectrum"
  )
}

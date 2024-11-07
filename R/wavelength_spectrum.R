#' Create a wavelength spectrum
#'
#' @param wavelength A numeric vector of wavelengths
#' @param amplitude A numeric vector of amplitudes corresponding to each wavelength
#'
#' @return An object of class "wavelength_spectrum" containing the wavelengths and amplitudes
#' @export
wavelength_spectrum <- function(wavelength, amplitude) {
  # Validate inputs
  if (length(wavelength) != length(amplitude)) {
    stop("wavelength and amplitude must be the same length")
  }

  # Return the structured object
  structure(
    list(wavelength = wavelength, amplitude = amplitude),
    class = "wavelength_spectrum"
  )
}

#' Create a waveform
#'
#' @param frequency_spectrum An object of class "frequency_spectrum" containing frequencies and amplitudes
#' @param wavelength_spectrum An object of class "wavelength_spectrum" containing wavelengths and amplitudes
#' @param phase A numeric value representing the phase of the waveform
#'
#' @return An object of class "waveform" containing the frequency spectrum, wavelength spectrum, and phase
#' @export
waveform <- function(frequency_spectrum, wavelength_spectrum, phase) {
  # Validate inputs
  if (!inherits(frequency_spectrum, "frequency_spectrum")) {
    stop("frequency_spectrum must be of class 'frequency_spectrum'")
  }
  if (!inherits(wavelength_spectrum, "wavelength_spectrum")) {
    stop("wavelength_spectrum must be of class 'wavelength_spectrum'")
  }
  if (!is.numeric(phase) || length(phase) != 1) {
    stop("phase must be a single numeric value")
  }

  # Return the structured object
  structure(
    list(
      frequency_spectrum = frequency_spectrum,
      wavelength_spectrum = wavelength_spectrum,
      phase = phase
    ),
    class = "waveform"
  )
}

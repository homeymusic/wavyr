#' Create a general waveform
#'
#' @param frequency_spectrum An object of class "frequency_spectrum" containing frequencies and amplitudes.
#' @param wavelength_spectrum Optional: An object of class "wavelength_spectrum" for custom wavelength components.
#' @param phase Optional: A numeric value representing the phase of the waveform.
#'
#' @return An object of class "waveform" containing the frequency spectrum, wavelength spectrum, and phase.
#' @export
waveform <- function(frequency_spectrum, wavelength_spectrum = NULL, phase = NULL) {
  # Validate inputs
  if (!inherits(frequency_spectrum, "frequency_spectrum")) {
    stop("frequency_spectrum must be of class 'frequency_spectrum'")
  }
  if (!is.null(wavelength_spectrum) && !inherits(wavelength_spectrum, "wavelength_spectrum")) {
    stop("wavelength_spectrum must be of class 'wavelength_spectrum'")
  }
  if (!is.null(phase) && (!is.numeric(phase) || length(phase) != 1)) {
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

#' @export
print.waveform <- function(x, ...) {
  cat("General Waveform\n")
  cat("Frequency Spectrum:\n")
  print(x$frequency_spectrum)
  if (!is.null(x$wavelength_spectrum)) {
    cat("Wavelength Spectrum:\n")
    print(x$wavelength_spectrum)
  }
  if (!is.null(x$phase)) {
    cat("Phase:", x$phase, "\n")
  }
}

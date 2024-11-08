#' Create a waveform
#'
#' @param frequency_spectrum An object of class "frequency_spectrum" containing frequencies and amplitudes
#' @param wavelength_spectrum (Optional) An object of class "wavelength_spectrum" containing wavelengths and amplitudes
#' @param phase (Optional) A numeric value representing the phase of the waveform
#' @param speed_of_sound (Optional) Numeric, speed of sound in the medium (used only if wavelength_spectrum is not provided)
#'
#' @return An object of class "waveform" containing the frequency spectrum, wavelength spectrum, and phase
#' @export
waveform <- function(frequency_spectrum, wavelength_spectrum = NULL, phase = NULL, speed_of_sound = 343) {
  # Validate frequency_spectrum input
  if (!inherits(frequency_spectrum, "frequency_spectrum")) {
    stop("frequency_spectrum must be of class 'frequency_spectrum'")
  }

  # If wavelength_spectrum is not provided, calculate it
  if (is.null(wavelength_spectrum)) {
    wavelengths <- speed_of_sound / frequency_spectrum$component

    # Calculate beat wavelengths
    beat_wavelengths <- c()
    beat_amplitudes <- c()
    for (i in seq_along(wavelengths)) {
      for (j in (i + 1):length(wavelengths)) {
        beat_wavelength <- abs(wavelengths[i] - wavelengths[j])
        beat_amplitude <- frequency_spectrum$amplitude[i] + frequency_spectrum$amplitude[j]
        beat_wavelengths <- c(beat_wavelengths, beat_wavelength)
        beat_amplitudes <- c(beat_amplitudes, beat_amplitude)
      }
    }

    # Create wavelength_spectrum object with both primary wavelengths and beat wavelengths
    wavelength_spectrum <- wavelength_spectrum(
      wavelength = c(wavelengths, beat_wavelengths),
      amplitude = c(frequency_spectrum$amplitude, beat_amplitudes)
    )
  } else {
    # Validate wavelength_spectrum input if it is provided
    if (!inherits(wavelength_spectrum, "wavelength_spectrum")) {
      stop("wavelength_spectrum must be of class 'wavelength_spectrum'")
    }
  }

  # Check if phase is provided; if not, set a default
  if (is.null(phase)) {
    phase <- 0  # or any default value you'd like
  } else if (!is.numeric(phase) || length(phase) != 1) {
    stop("phase must be a single numeric value")
  }

  # Return the structured waveform object
  structure(
    list(
      frequency_spectrum = frequency_spectrum,
      wavelength_spectrum = wavelength_spectrum,
      phase = phase
    ),
    class = "waveform"
  )
}

#' Create a LinearWaveform for a linear medium
#'
#' @param frequency_spectrum An object of class "frequency_spectrum" containing frequencies and amplitudes.
#' @param speed_of_sound Numeric, the speed of sound in the medium (e.g., 343 for air in m/s).
#'
#' @return An object of class "LinearWaveform" with frequency and wavelength spectra, and optional phase.
#' @export
linear_waveform <- function(frequency_spectrum, speed_of_sound = 343) {
  # Validate frequency_spectrum input
  if (!inherits(frequency_spectrum, "frequency_spectrum")) {
    stop("frequency_spectrum must be of class 'frequency_spectrum'")
  }

  # Calculate wavelengths for each frequency component
  wavelengths <- speed_of_sound / frequency_spectrum$component

  # Calculate beat wavelengths and amplitudes
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

  # Create the wavelength spectrum object
  wavelength_spectrum_obj <- wavelength_spectrum(
    wavelength = c(wavelengths, beat_wavelengths),
    amplitude = c(frequency_spectrum$amplitude, beat_amplitudes)
  )

  # Construct the LinearWaveform as a specialized waveform
  general_waveform <- waveform(
    frequency_spectrum = frequency_spectrum,
    wavelength_spectrum = wavelength_spectrum_obj
  )

  # Set the class to "LinearWaveform" inheriting "waveform"
  class(general_waveform) <- c("LinearWaveform", class(general_waveform))

  return(general_waveform)
}

#' @export
print.LinearWaveform <- function(x, ...) {
  cat("Linear Waveform in a Linear Medium\n")
  print.waveform(x)
}

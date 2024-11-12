#' Create a LinearWaveform for a linear medium
#'
#' @param frequency_spectrum An object of class "frequency_spectrum" containing frequencies and amplitudes.
#' @param wavelength_spectrum Optional: An object of class "wavelength_spectrum" for custom wavelength components.
#' @param phase Optional: A numeric value representing the phase of the waveform.
#'
#' @return An object of class "linear_waveform" with combined wavelength and frequency spectra, and beat spectrum.
#' @export
linear_waveform <- function(
    frequency_spectrum,
    wavelength_spectrum = NULL,
    phase = 0
) {

  # Validate frequency_spectrum input
  if (!inherits(frequency_spectrum, "frequency_spectrum")) {
    stop("frequency_spectrum must be of class 'frequency_spectrum'")
  }

  if (any(is.na(frequency_spectrum$component)) || any(frequency_spectrum$component <= 0)) {
    stop("All frequency components must be positive and non-NA.")
  }

  # Check amplitude values are positive
  if (any(is.na(frequency_spectrum$amplitude)) || any(frequency_spectrum$amplitude <= 0)) {
    stop("All amplitude values must be positive and non-NA.")
  }

  # Generate or validate the wavelength spectrum
  if (is.null(wavelength_spectrum)) {
    # Calculate base wavelength spectrum if not provided
    base_wavelength_spectrum <- wavelength_spectrum(
      wavelength = SPEED_OF_SOUND / frequency_spectrum$component,
      amplitude = frequency_spectrum$amplitude
    )
  } else {
    if (!inherits(wavelength_spectrum, "wavelength_spectrum")) {
      stop("wavelength_spectrum must be of class 'wavelength_spectrum'")
    }
    # Ensure that the size of wavelength_spectrum is greater than or equal to frequency_spectrum
    if (length(wavelength_spectrum$wavelength) < length(frequency_spectrum$frequency)) {
      stop("wavelength_spectrum must have a size greater than or equal to frequency_spectrum")
    }
    base_wavelength_spectrum <- wavelength_spectrum
  }

  # Calculate beat spectrum based on base wavelength spectrum
  beat_spectrum <- compute_beats_cpp(
    wavelength = base_wavelength_spectrum$component,
    amplitude = base_wavelength_spectrum$amplitude
  ) %>% wavelength_spectrum()

  if (is.null(beat_spectrum)) {
    stop("Failed to create beat spectrum.")
  }

  # Combine the base wavelength spectrum and beat spectrum
  combined_wavelength_spectrum <- combine_spectra(
    base_wavelength_spectrum,
    beat_spectrum,
    tolerance = FLOATING_POINT_TOLERANCE
  )

  waveform_obj <- waveform(frequency_spectrum, combined_wavelength_spectrum, phase)

  waveform_obj$base_wavelength_spectrum = base_wavelength_spectrum
  waveform_obj$beat_spectrum = beat_spectrum

  # Assign the class to include "linear_waveform"
  class(waveform_obj) <- c("linear_waveform", "waveform", "list")

  return(waveform_obj)
}

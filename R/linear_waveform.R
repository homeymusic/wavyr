#' Create a LinearWaveform for a linear medium
#'
#' @param frequency_spectrum An object of class "frequency_spectrum" containing frequencies and amplitudes.
#' @param speed_of_sound Numeric, the speed of sound in the medium (e.g., 343 for air in m/s).
#'
#' @return An object of class "linear_waveform" with combined wavelength and frequency spectra, and beat spectrum.
#' @export
linear_waveform <- function(
    frequency_spectrum,
    speed_of_sound = 343
) {

  # Validate frequency_spectrum input
  if (!inherits(frequency_spectrum, "frequency_spectrum")) {
    stop("frequency_spectrum must be of class 'frequency_spectrum'")
  }

  if (any(is.na(frequency_spectrum$component)) || any(frequency_spectrum$component <= 0)) {
    stop("All frequency components must be positive and non-NA.")
  }

  # Calculate base wavelength spectrum
  base_wavelength_spectrum <- wavelength_spectrum(
    wavelength = speed_of_sound / frequency_spectrum$component,
    amplitude = frequency_spectrum$amplitude
  )

  if (is.null(base_wavelength_spectrum) || is.null(base_wavelength_spectrum$component)) {
    stop("Failed to create base wavelength spectrum.")
  }

  # Calculate beat spectrum
  beat_spectrum <- compute_beats(
    wavelength = base_wavelength_spectrum$component,
    amplitude = base_wavelength_spectrum$amplitude
  ) %>% wavelength_spectrum()

  if (is.null(beat_spectrum)) {
    stop("Failed to create beat spectrum.")
  }

  # Combine the base wavelength spectrum and beat spectrum
  combined_wavelength_spectrum <- base_wavelength_spectrum$combine_with(beat_spectrum)

  # Construct the waveform object
  waveform_obj <- waveform(
    frequency_spectrum = frequency_spectrum,
    wavelength_spectrum = combined_wavelength_spectrum
  )

  # Assign the class to include "linear_waveform"
  class(waveform_obj) <- c("linear_waveform", class(waveform_obj))

  return(waveform_obj)
}

#' @export
print.linear_waveform <- function(x, ...) {
  cat("Linear Waveform in a Linear Medium\n")
  print.waveform(x)
  if (!is.null(x$beat_spectrum)) {
    cat("Beat Spectrum:\n")
    print(x$beat_spectrum)
  }
}


# structure(
#   list(
#     combined_waveform = combined_waveform,
#     frequency_spectrum = frequency_spectrum,
#     wavelength_spectrum = wavelength_spectrum,
#     base_wavelength_spectrum = base_wavelength_spectrum,
#     beat_spectrum = beat_spectrum,
#     phase = NULL
#   ),
#   class = c("LinearWaveform", class(.))
# )

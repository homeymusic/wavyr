#' Create a LinearWaveform for a linear medium
#'
#' @param frequency_spectrum An object of class "frequency_spectrum" containing frequencies and amplitudes.
#' @param speed_of_sound Numeric, the speed of sound in the medium (e.g., 343 for air in m/s).
#'
#' @return An object of class "LinearWaveform" with frequency and wavelength spectra, beat spectrum, and optional phase.
#' @export
linear_waveform <- function(
    frequency_spectrum,
    speed_of_sound = 343
) {
  # Validate frequency_spectrum input
  if (!inherits(frequency_spectrum, "frequency_spectrum")) {
    stop("frequency_spectrum must be of class 'frequency_spectrum'")
  }

  # Step 1: Calculate wavelengths for each frequency component
  wavelengths <- frequency_spectrum$component %>%
    {speed_of_sound / .}

  # Step 2: Calculate beat wavelengths and amplitudes using the Rcpp compute_beats function
  beat_spectrum <- compute_beats(
    wavelength = wavelengths,
    amplitude = frequency_spectrum$amplitude
  ) %>%
    wavelength_spectrum()

  # Step 3: Create the wavelength spectrum object, including both components and beat wavelengths
  wavelength_spectrum <- wavelength_spectrum(
    wavelength = c(wavelengths, beat_spectrum$component),
    amplitude = c(frequency_spectrum$amplitude, beat_spectrum$amplitude)
  )

  # Step 4: Construct the LinearWaveform as a specialized waveform
  combined_waveform <- waveform(
    frequency_spectrum = frequency_spectrum,
    wavelength_spectrum = wavelength_spectrum
  ) %>%
    {structure(
      list(
        frequency_spectrum = frequency_spectrum,
        wavelength_spectrum = wavelength_spectrum,
        beat_spectrum = beat_spectrum, # Add the beat_spectrum to the object
        phase = NULL
      ),
      class = c("LinearWaveform", class(.))
    )}

  return(combined_waveform)
}

#' @export
print.LinearWaveform <- function(x, ...) {
  cat("Linear Waveform in a Linear Medium\n")
  print.waveform(x)
  if (!is.null(x$beat_spectrum)) {
    cat("Beat Spectrum:\n")
    print(x$beat_spectrum)
  }
}

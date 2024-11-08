#' Create a LinearWaveform for a linear medium
#'
#' @param frequency_spectrum An object of class "frequency_spectrum" containing frequencies and amplitudes.
#' @param wavelength_spectrum Optional: An object of class "wavelength_spectrum" for custom wavelength components.
#' @param phase Optional: A numeric value representing the phase of the waveform.
#' @param speed_of_sound Numeric, the speed of sound in the medium (e.g., 343 for air in m/s).
#'
#' @return An object of class "linear_waveform" with combined wavelength and frequency spectra, and beat spectrum.
#' @export
linear_waveform <- function(
    frequency_spectrum,
    wavelength_spectrum = NULL,
    phase = 0,
    speed_of_sound = 343
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
      wavelength = speed_of_sound / frequency_spectrum$component,
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
    tolerance = 1e-6
  )

  indexed_spectra <- purrr::map2_dfr(
    combined_wavelength_spectrum$wavelength,
    combined_wavelength_spectrum$amplitude,
    function(wavelength, wavelength_amplitude) {
      # Calculate the equivalent frequency
      equivalent_frequency <- speed_of_sound / wavelength

      # Find any matching frequency within tolerance
      matched_indices <- which(abs(frequency_spectrum$frequency - equivalent_frequency) < 1e-6)

      # Construct the tibble for matched or unmatched cases
      tibble::tibble(
        frequency = if (length(matched_indices) > 0) frequency_spectrum$frequency[matched_indices][1] else NA,
        frequency_amplitude = if (length(matched_indices) > 0) {
          sum(frequency_spectrum$amplitude[matched_indices])
        } else NA,
        wavelength = wavelength,
        wavelength_amplitude = wavelength_amplitude
      )
    }
  ) %>% dplyr::arrange(dplyr::desc(wavelength))

  # Construct the waveform object with metadata
  waveform_obj <- list(
    frequency_spectrum = frequency_spectrum,
    wavelength_spectrum = combined_wavelength_spectrum,
    base_wavelength_spectrum = base_wavelength_spectrum,
    beat_spectrum = beat_spectrum,
    indexed_spectra = indexed_spectra,
    phase = phase
  )

  # Assign the class to include "linear_waveform"
  class(waveform_obj) <- c("linear_waveform", "waveform", "list")

  return(waveform_obj)
}

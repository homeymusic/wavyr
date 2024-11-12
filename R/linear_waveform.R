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
  beat_wavelength_spectrum <- compute_beats_cpp(
    wavelength = base_wavelength_spectrum$component,
    amplitude = base_wavelength_spectrum$amplitude
  ) %>% wavelength_spectrum()

  if (is.null(beat_wavelength_spectrum)) {
    stop("Failed to create beat spectrum.")
  }

  # Combine the base wavelength spectrum and beat spectrum
  combined_wavelength_spectrum <- combine_spectra(
    base_wavelength_spectrum,
    beat_wavelength_spectrum,
    tolerance = FLOATING_POINT_TOLERANCE
  )

  waveform_obj <- waveform(frequency_spectrum, combined_wavelength_spectrum, phase)

  waveform_obj$base_wavelength_spectrum = base_wavelength_spectrum
  waveform_obj$beat_wavelength_spectrum = beat_wavelength_spectrum

  # Assign the class to include "linear_waveform"
  class(waveform_obj) <- c("linear_waveform", "waveform", "list")

  return(waveform_obj)
}

#' Plot method for linear_waveform
#'
#' Overrides the plot function for linear_waveform to include beat_wavelength_spectrum as an overlay.
#' Calls the superclass plot method, while passing the beat_wavelength_spectrum to be displayed on top of the base wavelength spectrum.
#'
#' @param x A linear_waveform object.
#' @param label A label for the plot.
#' @param space_time_range The range of space and time for plotting.
#' @param resolution Resolution of the 2D plot.
#' @param line_plot_resolution Resolution of the line plots.
#' @param beat_wavelength_spectrum_color Color for the beat wavelength spectrum overlay.
#' @param ... Additional parameters passed to the superclass plot method.
#'
#' @export
plot.linear_waveform <- function(x, label = '',
                                 space_time_range = 25,
                                 resolution = 300,
                                 line_plot_resolution = 1000,
                                 beat_wavelength_spectrum_color = colors_homey$beat, ...) {

  # Customize the wavelength spectrum plot to include beat_wavelength_spectrum overlay
  wavelength_spectrum_grob <- grid::grid.grabExpr(
    plot(
      x$base_wavelength_spectrum,
      title = paste(label, "~ Wavelength Spectrum"),
      beat_wavelength_spectrum = x$beat_wavelength_spectrum,
      beat_wavelength_spectrum_color = beat_wavelength_spectrum_color
    )
  )

  # Call the superclass plot method with customized wavelength spectrum grob
  plot.waveform(
    x = x,
    label = label,
    space_time_range = space_time_range,
    resolution = resolution,
    line_plot_resolution = line_plot_resolution,
    wavelength_spectrum_grob = wavelength_spectrum_grob,
    ...
  )
}

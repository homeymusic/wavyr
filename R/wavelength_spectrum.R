#' Create a Wavelength Spectrum
#'
#' Extends the `spectrum` class specifically for wavelength spectra.
#' Accepts either direct numeric vectors or a list containing `idealized_wavelength` and `amplitude`.
#'
#' @param idealized_wavelength Either a numeric vector of idealized_wavelengths or a list with named `idealized_wavelength` and `amplitude` vectors.
#' @param amplitude A numeric vector of amplitudes, if `idealized_wavelength` is a numeric vector.
#' @param reference_component To compute the fundamental wavelength
#' @return An object of class \code{wavelength_spectrum} that inherits from \code{spectrum}.
#' @export
wavelength_spectrum <- function(idealized_wavelength, amplitude = NULL, reference_component=NULL) {
  # Check for non-numeric or inconsistent input directly in wavelength_spectrum
  if (is.list(idealized_wavelength) && is.null(amplitude)) {
    if (!is.numeric(idealized_wavelength$idealized_wavelength) || !is.numeric(idealized_wavelength$amplitude)) {
      stop("Both `idealized_wavelength` and `amplitude` in the list must be numeric.")
    }
  } else if (!is.numeric(idealized_wavelength) || (!is.null(amplitude) && !is.numeric(amplitude))) {
    stop("Both `idealized_wavelength` and `amplitude` must be numeric vectors.")
  }

  # Delegate to `spectrum()` for main functionality
  if (is.list(idealized_wavelength) && is.null(amplitude)) {
    # Handle list input
    stopifnot(
      length(idealized_wavelength$idealized_wavelength) == length(idealized_wavelength$amplitude)
    )
    spectrum_obj <- spectrum(idealized_component = idealized_wavelength$idealized_wavelength,
                             amplitude = idealized_wavelength$amplitude,
                             reference_component = reference_component,
                             extent_rate = EXTENT_RATE$extent)
  } else {
    # Direct numeric vectors
    spectrum_obj <- spectrum(idealized_component = idealized_wavelength,
                             amplitude = amplitude,
                             reference_component = reference_component,
                             extent_rate = EXTENT_RATE$extent)
  }

  # Add wavelength-specific fields
  spectrum_obj$idealized_wavelength <- spectrum_obj$idealized_component
  spectrum_obj$rationalized_fundamental_wavelength <- spectrum_obj$rationalized_fundamental

  # Set class to wavelength_spectrum
  class(spectrum_obj) <- c("wavelength_spectrum", class(spectrum_obj))

  return(spectrum_obj)
}

#' Plot a wavelength spectrum with spikes using ggplot2 and theme_homey
#'
#' Provides setup for wavelength-specific details before calling the core plot function.
#'
#' @param x An object of class "wavelength_spectrum".
#' @param rectangles Optional: A numeric vector specifying positions for additional rectangles.
#' @param title An optional character string for the plot title.
#' @param beat_wavelength_spectrum An optional beat spectrum object of class "spectrum".
#' @param beat_wavelength_spectrum_color A color for the beat spectrum segments. Defaults to colors_homey$beat.
#'
#' @export
plot.wavelength_spectrum <- function(x, rectangles = numeric(0), title = NULL, beat_wavelength_spectrum = NULL, beat_wavelength_spectrum_color = colors_homey$beat, ...) {
  x_label <- "Wavelength (m)"
  segment_color <- colors_homey$minor

  # Call plot.spectrum, passing the beat spectrum and color as overlay parameters
  plot.spectrum(
    x = x,
    x_label = x_label,
    segment_color = segment_color,
    rectangles = rectangles,
    title = title,
    overlay_spectrum = beat_wavelength_spectrum,
    overlay_spectrum_color = beat_wavelength_spectrum_color,
    ...
  )
}

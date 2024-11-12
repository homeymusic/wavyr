#' Create a Wavelength Spectrum
#'
#' Extends the `spectrum` class specifically for wavelength spectra.
#' Accepts either direct numeric vectors or a list containing `wavelength` and `amplitude`.
#'
#' @param wavelength Either a numeric vector of wavelengths or a list with named `wavelength` and `amplitude` vectors.
#' @param amplitude A numeric vector of amplitudes, if `wavelength` is a numeric vector.
#' @return An object of class \code{wavelength_spectrum} that inherits from \code{spectrum}.
#' @export
wavelength_spectrum <- function(wavelength, amplitude = NULL) {
  # Check for non-numeric or inconsistent input directly in wavelength_spectrum
  if (is.list(wavelength) && is.null(amplitude)) {
    if (!is.numeric(wavelength$wavelength) || !is.numeric(wavelength$amplitude)) {
      stop("Both `wavelength` and `amplitude` in the list must be numeric.")
    }
  } else if (!is.numeric(wavelength) || (!is.null(amplitude) && !is.numeric(amplitude))) {
    stop("Both `wavelength` and `amplitude` must be numeric vectors.")
  }

  # Delegate to `spectrum()` for main functionality
  if (is.list(wavelength) && is.null(amplitude)) {
    # Handle list input
    stopifnot(
      length(wavelength$wavelength) == length(wavelength$amplitude)
    )
    spectrum_obj <- spectrum(component = wavelength$wavelength, amplitude = wavelength$amplitude)
  } else {
    # Direct numeric vectors
    spectrum_obj <- spectrum(component = wavelength, amplitude = amplitude)
  }

  # Add wavelength-specific fields
  spectrum_obj$wavelength <- spectrum_obj$component
  spectrum_obj$fundamental_wavelength <- spectrum_obj$fundamental_cycle_length / max(spectrum_obj$wavelength)  # Fixed line

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
#'
#' @export
plot.wavelength_spectrum <- function(x, rectangles = numeric(0), title = NULL, ...) {
  x_label <- "Wavelength (m)"
  segment_color <- colors_homey$minor
  .plot.spectrum(x, x_label, segment_color, rectangles, title)
}


#' Create a Frequency Spectrum
#'
#' Extends the `spectrum` class specifically for frequency spectra.
#' Accepts either direct numeric vectors or a list containing `frequency` and `amplitude`.
#'
#' @param idealized_frequency Either a numeric vector of frequencies or a list with named `idealized_frequency` and `amplitude` vectors.
#' @param amplitude A numeric vector of amplitudes, if `idealized_frequency` is a numeric vector.
#' @param reference_component Used to compute the fundamental frequency from the cycle length
#' @return An object of class \code{frequency_spectrum} that inherits from \code{spectrum}.
#' @export
frequency_spectrum <- function(idealized_frequency, amplitude = NULL, reference_component = NULL) {
  # Check for non-numeric or inconsistent input directly in frequency_spectrum
  if (is.list(idealized_frequency) && is.null(amplitude)) {
    if (!is.numeric(idealized_frequency$idealized_frequency) || !is.numeric(idealized_frequency$amplitude)) {
      stop("Both `idealized_frequency` and `amplitude` in the list must be numeric.")
    }
  } else if (!is.numeric(idealized_frequency) || (!is.null(amplitude) && !is.numeric(amplitude))) {
    stop("Both `idealized_frequency` and `amplitude` must be numeric vectors.")
  }

  # Delegate to `spectrum()` for main functionality
  if (is.list(idealized_frequency) && is.null(amplitude)) {
    # Handle list input
    stopifnot(
      length(idealized_frequency$idealized_frequency) == length(idealized_frequency$amplitude)
    )
    spectrum_obj <- spectrum(idealized_component = idealized_frequency$idealized_frequency,
                             amplitude = idealized_frequency$amplitude,
                             reference_component = reference_component,
                             extent_rate = EXTENT_RATE$rate)
  } else {
    # Direct numeric vectors
    spectrum_obj <- spectrum(idealized_component = idealized_frequency,
                             amplitude = amplitude,
                             reference_component = reference_component,
                             extent_rate = EXTENT_RATE$rate)
  }

  # Add frequency-specific fields
  spectrum_obj$idealized_frequency <- spectrum_obj$idealized_component
  spectrum_obj$rationalized_rationalized_fundamental <- spectrum_obj$rationalized_fundamental

  # Set class to frequency_spectrum
  class(spectrum_obj) <- c("frequency_spectrum", class(spectrum_obj))

  return(spectrum_obj)
}

#' Plot a frequency spectrum with spikes using ggplot2 and theme_homey
#'
#' Provides setup for frequency-specific details before calling the core plot function.
#'
#' @param x An object of class "frequency_spectrum".
#' @param rectangles Optional: A numeric vector specifying positions for additional rectangles.
#' @param title An optional character string for the plot title.
#'
#' @export
plot.frequency_spectrum <- function(x, rectangles = numeric(0), title = NULL, ...) {
  x_label <- "Frequency (Hz)"
  segment_color <- colors_homey$major
  plot.spectrum(x, x_label, segment_color, rectangles, title)
}

#' Create a Frequency Heisen Spectrum
#'
#' Extends the `frequency_spectrum` class specifically for frequency spectra.
#' Accepts either direct numeric vectors or a list containing `frequency` and `amplitude`.
#'
#' @param frequency Either a numeric vector of frequencies or a list with named `frequency` and `amplitude` vectors.
#' @param amplitude A numeric vector of amplitudes, if `frequency` is a numeric vector.
#' @param reference_component Used to compute the fundamental frequency from the cycle length
#' @return An object of class \code{frequency_heisen_spectrum} that inherits from \code{spectrum}.
#' @export
frequency_heisen_spectrum <- function(frequency, amplitude = NULL, reference_component = NULL) {


  frequency_spectrum_obj <- frequency_spectrum(frequency = frequency,
                                     amplitude = amplitude,
                                     reference_component = reference_component)


  # Use heisen components
  frequency_spectrum_obj$component = frequency_spectrum_obj$heisen_component
  frequency_spectrum_obj$signal_component = frequency_spectrum_obj$signal_heisen_component

  # Add frequency-specific fields
  frequency_spectrum_obj$frequency <- frequency_spectrum_obj$component
  frequency_spectrum_obj$fundamental_frequency <- frequency_spectrum_obj$fundamental_component

  # Set class to frequency_heisen_spectrum
  class(frequency_spectrum_obj) <- c("frequency_heisen_spectrum", class(frequency_spectrum_obj))

  return(frequency_spectrum_obj)
}

#' Plot a frequency spectrum with spikes using ggplot2 and theme_homey
#'
#' Provides setup for frequency-specific details before calling the core plot function.
#'
#' @param x An object of class "frequency_heisen_spectrum".
#' @param rectangles Optional: A numeric vector specifying positions for additional rectangles.
#' @param title An optional character string for the plot title.
#'
#' @export
plot.frequency_heisen_spectrum <- function(x, rectangles = numeric(0), title = NULL, ...) {
  x_label <- "Frequency (Hz)"
  segment_color <- colors_homey$major
  plot.spectrum(x, x_label, segment_color, rectangles, title)
}

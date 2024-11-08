#' Create a Frequency Spectrum
#'
#' Extends the `spectrum` class specifically for frequency spectra.
#' Accepts either direct numeric vectors or a list containing `frequency` and `amplitude`.
#'
#' @param frequency Either a numeric vector of frequencies or a list with named `frequency` and `amplitude` vectors.
#' @param amplitude A numeric vector of amplitudes, if `frequency` is a numeric vector.
#' @return An object of class \code{frequency_spectrum} that inherits from \code{spectrum}.
#' @export
frequency_spectrum <- function(frequency, amplitude = NULL) {
  # Check for non-numeric or inconsistent input directly in frequency_spectrum
  if (is.list(frequency) && is.null(amplitude)) {
    if (!is.numeric(frequency$frequency) || !is.numeric(frequency$amplitude)) {
      stop("Both `frequency` and `amplitude` in the list must be numeric.")
    }
  } else if (!is.numeric(frequency) || (!is.null(amplitude) && !is.numeric(amplitude))) {
    stop("Both `frequency` and `amplitude` must be numeric vectors.")
  }

  # Delegate to `spectrum()` for main functionality
  if (is.list(frequency) && is.null(amplitude)) {
    # Handle list input
    stopifnot(
      length(frequency$frequency) == length(frequency$amplitude)
    )
    spectrum_obj <- spectrum(component = frequency$frequency, amplitude = frequency$amplitude)
  } else {
    # Direct numeric vectors
    spectrum_obj <- spectrum(component = frequency, amplitude = amplitude)
  }

  # Add wavelength field that references component directly
  spectrum_obj$frequency <- spectrum_obj$component

  # Set class to frequency_spectrum
  class(spectrum_obj) <- c("frequency_spectrum", class(spectrum_obj))

  return(spectrum_obj)
}

#' Plot a frequency_spectrum with spikes
#'
#' @param x An object of class "frequency_spectrum" containing component frequencies and amplitudes.
#' @param ... Additional parameters for plotting.
#'
#' @export
plot.frequency_spectrum <- function(x, ...) {

  # Create a spike plot
  plot(
    x$frequency, x$amplitude,
    type = "n",  # Set up the plot without points or lines
    xlab = "Frequency", ylab = "Amplitude",
    main = "Frequency Spectrum",
    ...
  )

  # Draw spikes
  segments(
    x0 = x$frequency, y0 = 0,
    x1 = x$frequency, y1 = x$amplitude,
    lwd = 2
  )
}

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

  # Add wavelength field that references component directly
  spectrum_obj$wavelength <- spectrum_obj$component

  # Set class to wavelength_spectrum
  class(spectrum_obj) <- c("wavelength_spectrum", class(spectrum_obj))

  return(spectrum_obj)
}

#' Plot a wavelength_spectrum with spikes
#'
#' @param x An object of class "wavelength_spectrum" containing component frequencies and amplitudes.
#' @param ... Additional parameters for plotting.
#'
#' @export
plot.wavelength_spectrum <- function(x, ...) {

  # Create a spike plot
  plot(
    x$wavelength, x$amplitude,
    type = "n",  # Set up the plot without points or lines
    xlab = "Wavelength", ylab = "Amplitude",
    main = "Wavelength Spectrum",
    ...
  )

  # Draw spikes
  segments(
    x0 = x$wavelength, y0 = 0,
    x1 = x$wavelength, y1 = x$amplitude,
    lwd = 2
  )
}

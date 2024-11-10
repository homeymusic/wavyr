#' Spectrum Constructor
#'
#' Creates a spectrum object with components and amplitudes.
#' Handles both direct vector inputs or a list containing named `component` and `amplitude`.
#'
#' @param component Either a numeric vector for component values or a list with named `component` and `amplitude` vectors.
#' @param amplitude A numeric vector of amplitudes, if `component` is a numeric vector.
#'
#' @return An object of class \code{spectrum}.
#' @export
spectrum <- function(component, amplitude = NULL) {
  if (is.list(component) && is.null(amplitude)) {
    UseMethod("spectrum", component)
  } else if (is.numeric(component) && is.numeric(amplitude)) {
    spectrum.default(component = component, amplitude = amplitude)
  } else {
    stop("Invalid input: please provide either numeric `component` and `amplitude` vectors or a list with both.")
  }
}

#' @rdname spectrum
#' @export
spectrum.default <- function(component, amplitude) {
  .spectrum(component = component, amplitude = amplitude)
}

#' @export
spectrum.list <- function(x, ...) {
  stopifnot(
    length(x) == 2L,
    is.numeric(x[[1]]),
    is.numeric(x[[2]]),
    length(x[[1]]) == length(x[[2]])
  )
  .spectrum(component = x[[1]], amplitude = x[[2]])
}

#' Internal spectrum constructor with validation
#' @keywords internal
.spectrum <- function(component, amplitude) {
  # Validation checks
  if (!is.numeric(component) || !is.numeric(amplitude)) {
    stop("Both component and amplitude must be numeric.")
  }
  if (length(component) != length(amplitude)) {
    stop("Component and amplitude must be the same length.")
  }
  if (any(component <= 0)) {
    stop("All component values must be positive.")
  }

  fractions <- approximate_rational_fractions_cpp(
    component / min(component),
    1 / (4 * pi),
    0.11
  )

  # Define additional methods
  fundamental_cycle_length <- lcm_integers(fractions$den)

  # Return the spectrum object
  structure(
    list(
      component = component,
      amplitude = amplitude,
      cycle_length = fractions$den,
      fundamental_cycle_length = fundamental_cycle_length,
      fractions = fractions
    ),
    class = "spectrum"
  )
}

#' @export
print.spectrum <- function(x, ...) {
  cat("Spectrum Object\n")
  cat("Components:", x$component, "\n")
  cat("Amplitudes:", x$amplitude, "\n")
}

combine_spectra <- function(spectrum, other_spectrum, tolerance) {
  result <- combine_spectra_cpp(
    spectrum$component, spectrum$amplitude,
    other_spectrum$component, other_spectrum$amplitude,
    tolerance
  )

  # Determine the class of the calling object to return the correct subclass
  spectrum_class <- class(spectrum)[1]  # This gets the first class which should be the specific spectrum class

  if (spectrum_class == "wavelength_spectrum") {
    return(wavelength_spectrum(result$component, result$amplitude))
  } else if (spectrum_class == "frequency_spectrum") {
    return(frequency_spectrum(result$component, result$amplitude))
  } else if (spectrum_class == "spectrum") {
    return(spectrum(result$component, result$amplitude))
  } else {
    stop("Unsupported spectrum type.")
  }
}

#' Plot a spectrum with spikes
#'
#' Creates a spike plot for a spectrum (frequency or wavelength).
#'
#' @param x An object of class "spectrum" containing components and amplitudes.
#' @param ... Additional parameters for plotting.
#'
#' @export
plot.spectrum <- function(x, rectangles = numeric(0), ...) {

  # Determine the appropriate labels based on the class
  if (inherits(x, "frequency_spectrum")) {
    x_label <- "Frequency"
    components <- x$frequency  # Use the frequency component
  } else if (inherits(x, "wavelength_spectrum")) {
    x_label <- "Wavelength"
    components <- x$wavelength  # Use the wavelength component
  } else {
    stop("Unsupported spectrum type")
  }

  # Create a spike plot
  plot(
    components, x$amplitude,
    type = "n",  # Set up the plot without points or lines
    xlab = x_label, ylab = "Amplitude",
    main = paste(x_label, "Spectrum"),
    xlim = c(min(c(rectangles, components)), max(c(rectangles, components))),
    ...
  )

  # Get the plot's current x-axis limits (from usr parameter)
  plot_limits <- par("usr")
  x_left <- plot_limits[1]   # The left x limit
  x_right <- plot_limits[2]  # The right x limit
  plot_width <- x_right - x_left
  y_bottom <- plot_limits[3]   # The lower y limit
  y_top <- plot_limits[4]      # The upper y limit

  rect_width <- plot_width * 0.0075  # 1% of the total width for the width

  # Draw spikes
  segments(
    x0 = components, y0 = 0,
    x1 = components, y1 = x$amplitude,
    lwd = 2
  )

  # Draw rectangles at the positions given in 'rectangles'
  if (length(rectangles) > 0) {
    for (x_pos in rectangles) {
      # Draw the rectangle on top of the spikes
      rect(x_pos - rect_width / 2, y_bottom, x_pos + rect_width / 2, y_top,
           border = "red", lwd = 2, lty = 2)  # Dashed rectangle
    }
  }
}

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

#' Plot a spectrum with spikes using ggplot2 and theme_homey
#'
#' Creates a spike plot for a spectrum (frequency or wavelength).
#'
#' @param x An object of class "spectrum" containing components and amplitudes.
#' @param rectangles Optional: A numeric vector specifying positions for additional rectangles.
#' @param title An optional character string for the plot title.
#' @param ... Additional parameters for plotting.
#'
#' @export
plot.spectrum <- function(x, rectangles = numeric(0), title = NULL, ...) {

  # Determine the appropriate labels based on the class
  if (inherits(x, "frequency_spectrum")) {
    x_label <- "Frequency (Hz)"
    components <- x$component  # Frequency components
  } else if (inherits(x, "wavelength_spectrum")) {
    x_label <- "Wavelength (m)"
    components <- x$component  # Wavelength components
  } else {
    stop("Unsupported spectrum type")
  }

  # Set a default title if none is provided
  if (is.null(title)) {
    title <- paste(x_label, "Spectrum")
  }

  # Create a data frame for ggplot
  spectrum_data <- data.frame(component = components, amplitude = x$amplitude)

  # Plot using ggplot2
  p <- ggplot2::ggplot(spectrum_data, ggplot2::aes(x = component, y = amplitude)) +
    ggplot2::geom_segment(ggplot2::aes(xend = component, yend = 0), color = colors_homey$neutral, lwd = 1.5) +  # Use 'neutral' color for spikes
    ggplot2::scale_x_continuous(name = x_label) +
    ggplot2::scale_y_continuous(name = "Amplitude") +
    ggplot2::labs(title = title) +  # Set the dynamic title
    theme_homey()

  # Add optional rectangles if specified
  if (length(rectangles) > 0) {
    # Calculate a slight width for rectangles based on the range of components
    rect_width <- 0.005 * (max(components) - min(components))

    # Create a data frame for rectangles
    rectangle_data <- data.frame(
      xmin = rectangles - rect_width / 2,
      xmax = rectangles + rect_width / 2,
      ymin = 0,  # Start from zero
      ymax = max(spectrum_data$amplitude)  # Up to the maximum amplitude
    )

    p <- p + ggplot2::geom_rect(
      data = rectangle_data,
      mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,  # Do not inherit the spectrum_data aesthetics
      color = "red", linetype = "dashed", fill = NA
    )
  }

  print(p)
}

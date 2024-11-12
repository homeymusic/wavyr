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

#' Internal spectrum constructor with validation and component combination
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

  # Combine close components within the specified tolerance directly using combine_spectra_cpp
  combined_result <- combine_spectra_cpp(
    component, amplitude,
    tolerance = FLOATING_POINT_TOLERANCE
  )

  # Extract the reduced components and amplitudes
  component <- combined_result$component
  amplitude <- combined_result$amplitude

  # Calculate additional properties
  fractions <- approximate_rational_fractions_cpp(
    component / min(component),
    1 / (4 * pi),
    0.11
  )

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

#' Combine two spectrum objects within a specified tolerance
#'
#' Combines two spectrum objects or reduces components within a single spectrum.
#'
#' @param spectrum The spectrum object to combine
#' @param other_spectrum An additional spectrum object to combine (optional)
#' @param tolerance The tolerance within which components are considered equal
#' @return A new spectrum object with combined components and amplitudes
#' @export
combine_spectra <- function(spectrum, other_spectrum = NULL, tolerance) {
  # Concatenate components and amplitudes if other_spectrum is provided
  if (!is.null(other_spectrum)) {
    combined_components <- c(spectrum$component, other_spectrum$component)
    combined_amplitudes <- c(spectrum$amplitude, other_spectrum$amplitude)
  } else {
    combined_components <- spectrum$component
    combined_amplitudes <- spectrum$amplitude
  }

  # Call combine_spectra_cpp with concatenated vectors
  result <- combine_spectra_cpp(
    combined_components, combined_amplitudes,
    tolerance = tolerance
  )

  # Return the combined result as the appropriate spectrum class
  spectrum_class <- class(spectrum)[1]
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
    segment_color = colors_homey$major
  } else if (inherits(x, "wavelength_spectrum")) {
    x_label <- "Wavelength (m)"
    components <- x$component  # Wavelength components
    segment_color = colors_homey$minor
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
    ggplot2::geom_segment(ggplot2::aes(xend = component, yend = 0), color = segment_color, lwd = 1.5) +  # Use 'neutral' color for spikes
    ggplot2::scale_x_continuous(name = x_label) +
    ggplot2::scale_y_continuous(name = "") +
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
      color = colors_homey$fundamental, linetype = "dashed", fill = NA
    )
  }

  print(p)
}

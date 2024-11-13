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

#' Core plotting function for spectrum objects
#'
#' Creates a spike plot based on subclass-specific labels and colors.
#' Allows for an optional overlay spectrum with a separate color.
#'
#' @param x An object of class "spectrum".
#' @param x_label A string label for the x-axis, provided by the subclass.
#' @param segment_color A color for the spectrum segments, provided by the subclass.
#' @param rectangles Optional: A numeric vector specifying positions for additional rectangles.
#' @param title An optional character string for the plot title.
#' @param overlay_spectrum An optional overlay spectrum object of class "spectrum".
#' @param overlay_spectrum_color A color for the overlay spectrum segments. Required if overlay_spectrum is provided.
#'
plot.spectrum <- function(x, x_label, segment_color, rectangles = numeric(0), title = NULL, overlay_spectrum = NULL, overlay_spectrum_color = NULL) {

  # Check if overlay_spectrum is provided without overlay_spectrum_color
  if (!is.null(overlay_spectrum) && is.null(overlay_spectrum_color)) {
    stop("overlay_spectrum_color must be specified if overlay_spectrum is provided.")
  }

  # Set a default title if none is provided
  if (is.null(title)) {
    title <- paste(x_label, "Spectrum")
  }

  # Create a data frame for the main spectrum plot
  spectrum_data <- data.frame(component = x$component, amplitude = x$amplitude)


  get_matching_amplitude <- function(component_value) {
    match <- subset(spectrum_data, abs(component - component_value) <= FLOATING_POINT_TOLERANCE)
    if (nrow(match) > 0) {
      return(match$amplitude[1])  # Return the first matching amplitude
    } else {
      return(0)  # Return NA if no match is found
    }
  }

  if (!is.null(overlay_spectrum)) {
    # Apply the function to each component in overlay_spectrum
    overlay_data <- tibble::tibble(
      component = overlay_spectrum$component,
      amplitude = overlay_spectrum$amplitude,
      spectrum_amplitude = sapply(overlay_spectrum$component, get_matching_amplitude),
      total_amplitude = .data$amplitude + .data$spectrum_amplitude
    )
  }

  # Determine the maximum amplitude across both spectra
  max_amplitude <- c(spectrum_data$amplitude, if (!is.null(overlay_spectrum)) overlay_data$total_amplitude else 0) %>%
    ceiling() %>%
    max()

  # Plot using ggplot2 for the main spectrum
  p <- ggplot2::ggplot(spectrum_data, ggplot2::aes(x = component, y = amplitude)) +
    ggplot2::geom_segment(ggplot2::aes(xend = component, yend = 0), color = segment_color, lwd = 1.5) +
    ggplot2::scale_x_continuous(name = x_label) +
    ggplot2::scale_y_continuous(name = "", limits = c(0, max_amplitude)) +
    ggplot2::labs(title = title) +
    theme_homey()

  # Add overlay spectrum if provided
  if (!is.null(overlay_spectrum)) {
    # Add overlay spectrum with a different color and linetype
    p <- p +
      ggplot2::geom_segment(
        data = overlay_data,
        ggplot2::aes(x=component, xend=component,
                     y=spectrum_amplitude, yend=total_amplitude),
        color=overlay_spectrum_color
      ) +
      ggplot2::geom_point(
        data = overlay_data,
        ggplot2::aes(x=component, y=total_amplitude),
        color=overlay_spectrum_color,
        size=2
      )
  }

  # Add optional rectangles if specified
  if (length(rectangles) > 0) {
    rect_width <- 0.06 * (max(x$component) - min(x$component))
    rectangle_data <- data.frame(
      xmin = rectangles - rect_width / 2,
      xmax = rectangles + rect_width / 2,
      ymin = 0,
      ymax = max_amplitude  # Use the maximum amplitude across both spectra
    )

    p <- p + ggplot2::geom_rect(
      data = rectangle_data,
      mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      color = colors_homey$fundamental, linetype = "solid", fill = NA
    )
  }

  print(p)
}

# spectrum.R

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

  # Define additional methods
  fundamental_cycle_length <- function() {
    fractions <- approximate_rational_fractions(
      component / min(component),
      1 / (4 * pi),
      0.11
    )
    lcm_integers(fractions$den)
  }

  fractions <- function() {
    approximate_rational_fractions(
      component / min(component),
      1 / (4 * pi),
      0.11
    )
  }

  combine_with <- function(other_spectrum, tolerance = 1e-6) {
    combined_component <- c(component, other_spectrum$component)
    combined_amplitude <- c(amplitude, other_spectrum$amplitude)

    # Create a data frame and group within the tolerance
    combined_df <- data.frame(component = combined_component, amplitude = combined_amplitude)
    combined_df <- combined_df[order(combined_df$component), ]

    combined_df$group <- cumsum(c(TRUE, diff(combined_df$component) > tolerance))

    # Aggregate amplitudes within each group
    aggregated <- aggregate(amplitude ~ group, data = combined_df, FUN = sum)
    unique_components <- aggregate(component ~ group, data = combined_df, FUN = mean)

    # Return the combined spectrum
    spectrum(unique_components$component, aggregated$amplitude)
  }

  # Return the spectrum object
  structure(
    list(
      component = component,
      amplitude = amplitude,
      fundamental_cycle_length = fundamental_cycle_length,
      fractions = fractions,
      combine_with = combine_with
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

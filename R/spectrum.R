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
    length(x) == 2L,               # Ensure the list has two elements
    is.numeric(x[[1]]),            # Ensure the first element is numeric
    is.numeric(x[[2]]),            # Ensure the second element is numeric
    length(x[[1]]) == length(x[[2]]) # Ensure both vectors are the same length
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

  # Return the spectrum object
  structure(
    list(
      component = component,
      amplitude = amplitude,
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

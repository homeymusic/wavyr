#' Create a spectrum superclass
#'
#' This is a superclass for handling shared functionality of wavelength and frequency spectra.
#'
#' @param component A numeric vector representing the primary component (e.g., frequency or wavelength)
#' @param amplitude A numeric vector of amplitudes corresponding to each component
#' @return An object of class "spectrum" containing the components and amplitudes
#' @export
spectrum <- function(component, amplitude) {
  # Validate inputs
  if (length(component) != length(amplitude)) {
    stop("component and amplitude must be the same length")
  }

  # Define the ratios function to generate fractions
  fractions <- function() {
    approximate_rational_fractions(
      component / min(component),
      1 / (4 * pi),
      0.11
    )
  }

  # Define the fundamental_cycle_length method
  fundamental_cycle_length <- function() {
    lcm_integers(fractions()$den)
  }

  # Return the structured object
  structure(
    list(
      component                = component,
      amplitude                = amplitude,
      fundamental_cycle_length = fundamental_cycle_length,
      fractions                = fractions  # Expose ratios for external use if needed
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

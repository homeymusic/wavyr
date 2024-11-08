#' Create a wavelength spectrum
#'
#' @param wavelength A numeric vector of wavelengths
#' @param amplitude A numeric vector of amplitudes corresponding to each wavelength
#'
#' @return An object of class "wavelength_spectrum" containing the wavelengths and amplitudes
#' @export
wavelength_spectrum <- function(wavelength, amplitude) {
  # Validate inputs
  if (length(wavelength) != length(amplitude)) {
    stop("wavelength and amplitude must be the same length")
  }

  # Define the fundamental_cycle_length stub method
  fundamental_cycle_length <- function() {
    fractions = approximate_rational_fractions(
      wavelength / min(wavelength),
      1 / (4*pi),
      0.11
    )
    lcm_integers(fractions$den)
  }
  lcm_integers <- function(x) Reduce(gmp::lcm.bigz, x) %>% as.numeric()

  # Return the structured object
  structure(
    list(wavelength = wavelength,
         amplitude = amplitude,
         fundamental_cycle_length = fundamental_cycle_length),
    class = "wavelength_spectrum"
  )
}

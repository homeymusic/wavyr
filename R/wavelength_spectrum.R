#' Create a wavelength spectrum
#'
#' @param wavelengths A tibble with columns `wavelength` and `amplitude`
#'
#' @return An object of class "wavelength_spectrum" containing the wavelengths and amplitudes
#' @export
wavelength_spectrum <- function(wavelengths) {
  # Validate input
  if (!("wavelength" %in% names(wavelengths)) || !("amplitude" %in% names(wavelengths))) {
    stop("wavelengths must be a tibble with columns 'wavelength' and 'amplitude'")
  }

  structure(
    list(wavelengths = wavelengths),
    class = "wavelength_spectrum"
  )
}

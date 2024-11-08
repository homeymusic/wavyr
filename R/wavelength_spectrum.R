# wavelength_spectrum.R

#' Create a wavelength spectrum
#'
#' @param wavelength A numeric vector of wavelengths
#' @param amplitude A numeric vector of amplitudes
#' @return An object of class "wavelength_spectrum" that inherits from "spectrum"
#' @export
wavelength_spectrum <- function(wavelength, amplitude) {
  spectrum_obj <- spectrum(wavelength, amplitude)
  class(spectrum_obj) <- c("wavelength_spectrum", class(spectrum_obj))
  return(spectrum_obj)
}

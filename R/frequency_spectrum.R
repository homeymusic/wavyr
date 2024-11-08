# frequency_spectrum.R

#' Create a frequency spectrum
#'
#' @param frequency A numeric vector of frequencies
#' @param amplitude A numeric vector of amplitudes
#' @return An object of class "frequency_spectrum" that inherits from "spectrum"
#' @export
frequency_spectrum <- function(frequency, amplitude) {
  spectrum_obj <- spectrum(frequency, amplitude)
  class(spectrum_obj) <- c("frequency_spectrum", class(spectrum_obj))
  return(spectrum_obj)
}

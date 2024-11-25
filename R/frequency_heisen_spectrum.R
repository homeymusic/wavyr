#' Create a Frequency Heisen Spectrum
#'
#' Extends the `frequency_spectrum` class specifically for heisen spectra.
#' Accepts either direct numeric vectors or a list containing `frequency` and `amplitude`.
#'
#' @param frequency Either a numeric vector of frequencies or a list with named `frequency` and `amplitude` vectors.
#' @param amplitude A numeric vector of amplitudes, if `frequency` is a numeric vector.
#' @param reference Used to compute the fundamental frequency from the cycle length
#' @return An object of class \code{frequency_spectrum} that inherits from \code{spectrum}.
#' @export
frequency_heisen_spectrum <- function(frequency, amplitude = NULL, reference = NULL) {
  frequency_spectrum_obj <- frequency_spectrum(frequency, amplitude, reference)

    # Set class to frequency_spectrum
  class(frequency_spectrum_obj) <- c("frequency_heisen_spectrum", class(frequency_spectrum_obj))

  frequency_spectrum_obj$false_certainty_component = frequency_spectrum_obj$component


  browser()


  frequency_spectrum_obj$component = frequency_spectrum_obj$reference *
    frequency_spectrum_obj$fractions$approximation

  return(frequency_spectrum_obj)
}

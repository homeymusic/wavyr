#' Space Signal Constructor
#'
#' Creates a space_signal object from a wavelength_spectrum object.
#'
#' @param wavelength_spectrum An object of class "wavelength_spectrum" (subclass of "spectrum").
#'
#' @return An object of class \code{space_signal}.
#' @export
space_signal <- function(wavelength_spectrum) {
  # Ensure the input is a valid wavelength_spectrum object
  if (!inherits(wavelength_spectrum, "wavelength_spectrum")) {
    stop("Input must be of class 'wavelength_spectrum'")
  }

  # Use the signal constructor to create a base signal object
  signal_obj <- signal(wavelength_spectrum)

  signal_obj$wavelength_spectrum = signal_obj$spectrum

  signal_obj$plot_color = colors_homey$minor
  signal_obj$physical_label = 'Space'
  signal_obj$spectral_label = 'Wavelength'
  signal_obj$observable_label = 'Amplitude'

  # Create the space_signal object, inheriting everything from signal
  structure(
    signal_obj,
    class = c("space_signal", "signal")  # Class inheritance for space_signal and signal
  )
}

#' Time Signal Constructor
#'
#' Creates a time_signal object from a frequency_spectrum object.
#'
#' @param frequency_spectrum An object of class "frequency_spectrum" (subclass of "spectrum").
#'
#' @return An object of class \code{time_signal}.
#' @export
time_signal <- function(frequency_spectrum) {
  # Ensure the input is a valid frequency_spectrum object
  if (!inherits(frequency_spectrum, "frequency_spectrum")) {
    stop("Input must be of class 'frequency_spectrum'")
  }

  # Use the signal constructor to create a base signal object
  signal_obj <- signal(frequency_spectrum)

  signal_obj$frequency_spectrum = signal_obj$spectrum

  signal_obj$plot_color = colors_homey$major
  signal_obj$physical_label = 'Time'
  signal_obj$spectral_label = 'Frequency'
  signal_obj$observable_label = 'Amplitude'

  # Create the time_signal object, inheriting everything from signal
  structure(
    signal_obj,
    class = c("time_signal", "signal")  # Class inheritance for time_signal and signal
  )
}

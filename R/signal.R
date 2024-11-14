#' Signal Constructor
#'
#' Creates a signal object from a given spectrum object.
#'
#' @param spectrum An object of class "spectrum" (or one of its subclasses).
#'
#' @return An object of class \code{signal}.
#' @export
signal <- function(spectrum) {
  # Ensure the input is a valid spectrum object
  if (!inherits(spectrum, "spectrum")) {
    stop("Input must be of class 'spectrum'")
  }

  # Define the amplitude function
  amplitude_fn <- function(coordinate) {
    # Sum the contributions of each frequency component
    sum(spectrum$amplitude * cos(2 * pi * spectrum$component * coordinate))
  }

  # Create the signal object, including the amplitude function
  structure(
    list(
      spectrum = spectrum,
      amplitude = amplitude_fn  # Add the amplitude function to the signal object
    ),
    class = "signal"
  )
}

#' Print method for signal objects
#'
#' Provides a summary of the signal object.
#'
#' @param x A signal object.
#' @param ... Additional arguments (not used).
#' @export
print.signal <- function(x, ...) {
  cat("Signal Object\n")
  cat("Spectrum:\n")
  print(x$spectrum)  # Print the spectrum contained within the signal
}

#' Plot method for signal objects
#'
#' Creates a plot of the signal's amplitude over a specified coordinate range or a number of cycles.
#'
#' @param x A signal object.
#' @param label An optional label for the plot.
#' @param coordinate_range A numeric vector of length 2 specifying the start and stop values for the coordinate range.
#' @param number_of_cycles A numeric value specifying the number of cycles to plot. Defaults to 3 if not provided.
#' @param resolution Number of points to sample within the range.
#' @export
plot.signal <- function(x, label = '', coordinate_range = NULL, number_of_cycles = NULL, resolution = 300) {

  # Validate that both coordinate_range and number_of_cycles are not provided simultaneously
  if (!is.null(coordinate_range) && !is.null(number_of_cycles)) {
    stop("Please provide either 'coordinate_range' or 'number_of_cycles', not both.")
  }

  # Default to 3 cycles if number_of_cycles is NULL
  if (is.null(number_of_cycles)) {
    number_of_cycles <- 3
  }

  # Calculate the coordinate range based on number_of_cycles
  if (is.null(coordinate_range)) {
    # If coordinate_range is not provided, calculate it based on the fundamental cycle length
    fundamental_cycle_length <- x$spectrum$fundamental_cycle_length
    coordinate_range <- c(0, number_of_cycles * fundamental_cycle_length)
  }

  # Validate coordinate_range
  if (length(coordinate_range) != 2 || !is.numeric(coordinate_range)) {
    stop("coordinate_range must be a numeric vector of length 2.")
  }

  # Generate coordinate values based on the specified range and resolution
  coordinate_values <- seq(coordinate_range[1], coordinate_range[2], length.out = resolution)

  # Compute amplitudes over the coordinate values
  amplitude_values <- sapply(coordinate_values, x$amplitude)

  # Create data frame for plotting
  plot_data <- data.frame(
    coordinate = coordinate_values,
    amplitude = amplitude_values
  )

  # Create the plot using ggplot2
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = coordinate, y = amplitude)) +
    ggplot2::geom_line(color = colors_homey$neutral) +
    ggplot2::scale_x_continuous(name = "Coordinate") +
    ggplot2::labs(
      title = bquote(.(label) ~ "Signal Plot"),
      x = "Coordinate",
      y = "Amplitude"
    ) +
    ggplot2::scale_y_continuous(name = "Amplitude") +
    theme_homey()

  print(p)
}

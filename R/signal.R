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

  amplitude_fn <- function(coordinate) {
    # Calculate the sum of cosine components with the adjusted component
    sum(spectrum$amplitude * cos(2 * pi * spectrum$signal_component * coordinate))
  }

  plot_color = colors_homey$neutral
  physical_label = 'Coordinate'
  spectral_label = 'Spectrum'
  observable_label = 'Amplitude'
  physical_units = 'Natural Coordinate Units'
  observable_units = 'Natural Amplitude Units'
  spectral_units = 'Cycles per Natural Coordinate Unit'

  # Create the signal object, including the amplitude function
  structure(
    list(
      plot_color = plot_color,
      physical_label = physical_label,
      spectral_label = spectral_label,
      observable_label = observable_label,
      physical_units = physical_units,
      observable_units = observable_units,
      spectral_units = spectral_units,
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
  cat("Plot Color:", x$plot_color, "\n")
  cat("Spectrum Length:", x$spectrum %>% length(), "\n")
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
plot.signal <- function(x, title = '', coordinate_range = NULL, number_of_cycles = NULL, resolution = 300) {

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
    coordinate_range <- c(0, number_of_cycles * x$spectrum$fundamental_cycle_length)
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
    ggplot2::geom_line(color = x$plot_color) +
    ggplot2::scale_x_continuous(name = x$physical_label) +
    ggplot2::labs(
      title = bquote(.(title)),
      x = paste(x$physical_label, paste0('(', x$physical_units,')')),
      y = paste(x$observable_label, paste0('(', x$observable_units,')'))
    ) +
    ggplot2::scale_y_continuous(name = x$observable_label) +
    theme_homey()

  print(p)
}

#' Plot Detailed Signal for a Spectrum
#'
#' This function generates a detailed visualization of a spectrum's signal, including:
#' - A composite wave plot that combines all components of the spectrum.
#' - Individual plots for each component of the spectrum.
#' The plots are aligned to the same coordinate range, derived from the number of cycles of the spectrum's fundamental cycle length.
#'
#' @param x A spectrum object.
#' @param title A string specifying the title of the composite wave plot. Defaults to "Detailed Signal Plot".
#' @param number_of_cycles A numeric value specifying the number of cycles to display in the coordinate range. Defaults to 5.
#' @param resolution An integer specifying the number of points sampled within the coordinate range for plotting. Defaults to 1000.
#'
#' @details
#' The function uses `signal()` to generate the composite wave and individual component signals. Each plot uses the same coordinate range, ensuring visual consistency across all plots. The composite wave plot is displayed on top, followed by individual component plots in order of the components' indices.
#'
#' @return A combined plot using `cowplot::plot_grid`, with the composite wave on top and individual component plots below.
#'
#' @export
plot_details.signal <- function(x, title = '', number_of_cycles = 3, resolution = 1000) {
  # Define the coordinate range for all plots
  coordinate_range <- seq(
    0,
    number_of_cycles * x$spectrum$fundamental_cycle_length,
    length.out = resolution
  )

  # Create the composite wave plot
  composite_plot <- x %>%
    plot(title = paste(title,
                       'Fundamental',
                       paste0(x$spectral_label, ':'),
                       x$spectrum$fundamental_component,
                       paste0('(',x$spectral_units,')')),
                       coordinate_range = c(0, max(coordinate_range)))

  # Generate individual plots for each component in the spectrum
  individual_plots <- lapply(seq_along(x$spectrum$component), function(i) {
    # Create a single spectrum for the current component
    single_spectrum <- spectrum(
      component = x$spectrum$component[i],
      amplitude = x$spectrum$amplitude[i],
      inverted = x$spectrum$inverted,
      reference_component = x$reference_component
    )
    # Plot the signal of the single spectrum within the shared coordinate range
    single_spectrum %>%
      signal() %>%
      plot(title = paste(x$spectrum$component[i],
                         paste0('(',x$spectral_units,')')),
           coordinate_range = c(0, max(coordinate_range)))
  })

  # Combine the composite plot and individual plots using cowplot
  cowplot::plot_grid(
    composite_plot,
    cowplot::plot_grid(plotlist = individual_plots, ncol = 1),
    ncol = 1,
    rel_heights = c(1, length(individual_plots))  # Composite wave gets more space
  )
}

#' Heisen Signal Constructor
#'
#' Creates a time_signal object from a frequency_spectrum object.
#'
#' @param frequency_spectrum An object of class "frequency_spectrum" (subclass of "spectrum").
#'
#' @return An object of class \code{time_signal}.
#' @export
heisen_signal <- function(frequency_spectrum) {
  # Ensure the input is a valid frequency_spectrum object
  if (!inherits(frequency_spectrum, "frequency_spectrum")) {
    stop("Input must be of class 'frequency_spectrum'")
  }

  # Use the signal constructor to create a base signal object
  signal_obj <- signal(frequency_spectrum)

  # Create the time_signal object, inheriting everything from signal
  structure(
    signal_obj,
    class = c("heisen_signal", class(signal_obj))  # Class inheritance for time_signal and signal
  )
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
plot.heisen_signal <- function(x, title = '', coordinate_range = NULL, number_of_cycles = NULL, resolution = 99) {
print("HEISEN")
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
plot_details.heisen_signal <- function(x, title = '', coordinate_range = NULL, number_of_cycles = NULL, resolution = 99) {

  print('HEISEN')
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


  # Create the composite wave plot
  composite_plot <- x %>%
    plot(title = paste(title,
                       'Fundamental',
                       paste0(x$spectral_label, ':'),
                       sprintf("%.2f", x$spectrum$fundamental_component),
                       paste0('(',x$spectral_units,')')),
         coordinate_range = coordinate_range,
         resolution = resolution)

  # Generate individual plots for each component in the spectrum
  spectrum_type = get(class(x$spectrum)[1])
  individual_plots <- lapply(seq_along(x$spectrum$component), function(i) {
    # Create a single spectrum for the current component
    single_spectrum <- spectrum_type(
      x$spectrum$component[i],
      amplitude = x$spectrum$amplitude[i],
      reference = x$reference
    )

    # Plot the signal of the single spectrum within the shared coordinate range
    signal_type = get(class(x)[1])
    single_spectrum %>%
      signal_type() %>%
      plot(title = paste(
        paste0(x$spectral_label, ':'),
        sprintf("%.2f", x$spectrum$component[i]), paste0('(',x$spectral_units,')')),
        coordinate_range = coordinate_range,
        resolution = resolution)
  })

  # Combine composite_plot and individual_plots into a single list
  all_plots <- c(list(composite_plot), individual_plots)

  # Set relative heights: 2 for the first plot, 1 for each individual plot
  rel_heights <- c(2, rep(1, length(individual_plots)))

  # Use plot_grid with rel_heights to adjust the sizes
  print(cowplot::plot_grid(plotlist = all_plots, ncol = 1, rel_heights = rel_heights))
}

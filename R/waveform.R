waveform <- function(frequency_spectrum, wavelength_spectrum, phase = 0) {
  # Validate inputs
  if (!inherits(frequency_spectrum, "frequency_spectrum")) {
    stop("frequency_spectrum must be of class 'frequency_spectrum'")
  }
  if (!inherits(wavelength_spectrum, "wavelength_spectrum")) {
    stop("wavelength_spectrum must be of class 'wavelength_spectrum'")
  }
  if (!is.null(phase) && (!is.numeric(phase) || length(phase) != 1)) {
    stop("phase must be a single numeric value")
  }

  indexed_spectra <- purrr::pmap_dfr(
    list(
      wavelength = wavelength_spectrum$wavelength,
      wavelength_amplitude = wavelength_spectrum$amplitude,
      wavelength_cycle_length = wavelength_spectrum$cycle_length
    ),
    function(wavelength, wavelength_amplitude, wavelength_cycle_length) {
      equivalent_frequency <- SPEED_OF_SOUND / wavelength
      matched_indices <- which(abs(frequency_spectrum$frequency - equivalent_frequency) < 1e-6)

      tibble::tibble(
        frequency = if (length(matched_indices) > 0) frequency_spectrum$frequency[matched_indices][1] else NA,
        frequency_amplitude = if (length(matched_indices) > 0) sum(frequency_spectrum$amplitude[matched_indices]) else NA,
        frequency_cycle_length = if (length(matched_indices) > 0) frequency_spectrum$cycle_length[matched_indices][1] else NA,
        wavelength = wavelength,
        wavelength_amplitude = wavelength_amplitude,
        wavelength_cycle_length = wavelength_cycle_length
      )
    }
  ) %>% dplyr::arrange(dplyr::desc(wavelength))

  # Calculate the maximum amplitude for both fundamental and composite
  A0 <- max(frequency_spectrum$amplitude) + max(wavelength_spectrum$amplitude)

  composite_amplitude <- function(x, t) {
    # Check that x and t are scalar values
    if (length(x) != 1 || length(t) != 1) {
      stop("x and t must be scalar values")
    }

    # Calculate the composite amplitude as the sum of contributions
    sum(
      purrr::pmap_dbl(indexed_spectra, function(frequency,
                                                frequency_amplitude,
                                                frequency_cycle_length,
                                                wavelength,
                                                wavelength_amplitude,
                                                wavelength_cycle_length) {
        An <- ifelse(is.na(frequency_amplitude), 0, frequency_amplitude) +
          ifelse(is.na(wavelength_amplitude), 0, wavelength_amplitude)

        relative_fn <- ifelse(is.na(frequency_cycle_length), 0, 1 / frequency_cycle_length)
        relative_kn <- ifelse(is.na(wavelength_cycle_length), 0, 1 / wavelength_cycle_length)

        # Scale by the maximum amplitude A0 to keep it consistent
        (An / A0) * cos(2 * pi * (relative_fn * t - relative_kn * x) + phase)
      })
    ) * A0  # Rescale the sum to match the A0 range
  }

  fundamental_amplitude <- function(x, t) {
    # Check that x and t are scalar values
    if (length(x) != 1 || length(t) != 1) {
      stop("x and t must be scalar values")
    }

    # Calculate relative frequency and wavenumber
    relative_f0 <- 1 / frequency_spectrum$fundamental_cycle_length
    relative_k0 <- 1 / wavelength_spectrum$fundamental_cycle_length

    # Calculate the total amplitude as the sum of all amplitude contributions
    A0 <- sum(frequency_spectrum$amplitude) + sum(wavelength_spectrum$amplitude)

    # Compute and return the amplitude at (x, t)
    A0 * cos(2 * pi * relative_f0 * t - 2 * pi * relative_k0 * x + phase)
  }
  # Return the structured object
  structure(
    list(
      frequency_spectrum = frequency_spectrum,
      wavelength_spectrum = wavelength_spectrum,
      phase = phase,
      indexed_spectra = indexed_spectra,
      fundamental_amplitude = fundamental_amplitude,
      composite_amplitude = composite_amplitude
    ),
    class = "waveform"
  )
}

#' @export
plot.waveform <- function(x, label = '',
                          space_time_range = 25,
                          resolution = 300,  # Higher resolution for 2D plots
                          line_plot_resolution = 1000, ...) {  # Increased resolution for line plots

  f0 <- x$frequency_spectrum$fundamental_frequency
  k0 <- 1 / x$wavelength_spectrum$fundamental_wavelength

  time_fundamental_cycle_length <- x$frequency_spectrum$fundamental_cycle_length
  space_fundamental_cycle_length <- x$wavelength_spectrum$fundamental_cycle_length

  tonality <- if (time_fundamental_cycle_length > space_fundamental_cycle_length) {
    'minor'
  } else if (time_fundamental_cycle_length == space_fundamental_cycle_length) {
    'neutral'
  } else {
    'major'
  }

  color_set <- saturation_colors_homey[[tonality]]

  # Calculate time and space ranges
  max_time <- space_time_range / time_fundamental_cycle_length * (1 / f0)
  max_space <- space_time_range / space_fundamental_cycle_length * (1 / k0)

  # Define grid for 2D plots and values for line plots
  time_values <- seq(0, space_time_range, length.out = line_plot_resolution)  # Higher resolution for line plots
  space_values <- seq(0, space_time_range, length.out = line_plot_resolution)
  grid <- base::expand.grid(time = seq(0, space_time_range, length.out = resolution),
                            space = seq(0, space_time_range, length.out = resolution))

  # 2D Composite Amplitude
  grid$composite_amplitude <- mapply(
    function(space, time) x$composite_amplitude(space, time),
    grid$space, grid$time
  )
  composite_2d <- ggplot2::ggplot(grid, ggplot2::aes(x = time, y = space, fill = composite_amplitude)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(low = color_set$lo, high = color_set$hi, name = "Amplitude") +
    ggplot2::scale_x_continuous(name = "Time (s)", labels = function(x) sprintf("%.3f", x * max_time / space_time_range), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(name = "Space (m)", labels = function(y) sprintf("%.3f", y * max_space / space_time_range), expand = c(0, 0)) +
    ggplot2::labs(
      title = bquote(.(label) ~ "Composite")
    ) +
    ggplot2::coord_fixed(ratio = 1) +
    theme_homey()

  # Time-only Composite with computed axis limits
  time_only_composite <- data.frame(
    time = time_values,
    amplitude = sapply(time_values, function(t) x$composite_amplitude(0, t))
  )
  composite_time <- ggplot2::ggplot(time_only_composite, ggplot2::aes(x = time, y = amplitude)) +
    ggplot2::geom_line(color = color_set$hi) +
    ggplot2::scale_x_continuous(name = "Time (s)", limits = c(0, space_time_range), labels = function(x) sprintf("%.3f", x * max_time / space_time_range)) +
    ggplot2::labs(
      title = bquote(.(label) ~ "Composite Time Slice")
    ) +
    ggplot2::scale_y_continuous(name = "") +
    theme_homey()

  # Space-only Composite with computed axis limits
  space_only_composite <- data.frame(
    space = space_values,
    amplitude = sapply(space_values, function(s) x$composite_amplitude(s, 0))
  )
  composite_space <- ggplot2::ggplot(space_only_composite, ggplot2::aes(x = space, y = amplitude)) +
    ggplot2::geom_line(color = color_set$hi) +
    ggplot2::scale_x_continuous(name = "Space (m)", limits = c(0, space_time_range), labels = function(y) sprintf("%.3f", y * max_space / space_time_range)) +
    ggplot2::labs(
      title = bquote(.(label) ~ "Composite Space Slice")
    ) +
    ggplot2::scale_y_continuous(name = "") +
    theme_homey()

  # 2D Fundamental Amplitude
  grid$fundamental_amplitude <- mapply(
    function(space, time) x$fundamental_amplitude(space, time),
    grid$space, grid$time
  )

  fundamental_2d <- ggplot2::ggplot(grid, ggplot2::aes(x = time, y = space, fill = fundamental_amplitude)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(low = color_set$lo, high = color_set$hi, name = "Amplitude") +
    ggplot2::scale_x_continuous(name = "Time (s)", labels = function(x) sprintf("%.3f", x * max_time / space_time_range), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(name = "Space (m)", labels = function(y) sprintf("%.3f", y * max_space / space_time_range), expand = c(0, 0)) +
    ggplot2::labs(
      title = bquote(.(label) ~ "Fundamental f0:" ~ .(formatC(f0, format = "f", digits = 1)) ~ "Hz k0:" ~ .(formatC(k0, format = "f", digits = 1)) ~ m^-1)
    ) +
    ggplot2::coord_fixed(ratio = 1) +
    theme_homey()

  # Time-only Fundamental with computed axis limits
  time_only_fundamental <- data.frame(
    time = time_values,
    amplitude = sapply(time_values, function(t) x$fundamental_amplitude(0, t))
  )
  fundamental_time <- ggplot2::ggplot(time_only_fundamental, ggplot2::aes(x = time, y = amplitude)) +
    ggplot2::geom_line(color = color_set$hi) +
    ggplot2::scale_x_continuous(name = "Time (s)", limits = c(0, space_time_range), labels = function(x) sprintf("%.3f", x * max_time / space_time_range)) +
    ggplot2::labs(
      title = bquote(.(label) ~ "Fundamental Time Slice f0:" ~ .(formatC(f0, format = "f", digits = 1)) ~ "Hz")
    ) +
    ggplot2::scale_y_continuous(name = "") +
    theme_homey()

  # Space-only Fundamental with computed axis limits
  space_only_fundamental <- data.frame(
    space = space_values,
    amplitude = sapply(space_values, function(s) x$fundamental_amplitude(s, 0))
  )
  fundamental_space <- ggplot2::ggplot(space_only_fundamental, ggplot2::aes(x = space, y = amplitude)) +
    ggplot2::geom_line(color = color_set$hi) +
    ggplot2::scale_x_continuous(name = "Space (m)", limits = c(0, space_time_range), labels = function(y) sprintf("%.3f", y * max_space / space_time_range)) +
    ggplot2::labs(
      title = bquote(.(label) ~ "Fundamental Space Slice k0:" ~ .(formatC(k0, format = "f", digits = 1)) ~ m^-1)
    ) +
    ggplot2::scale_y_continuous(name = "") +
    theme_homey()

  frequency_spectrum_grob <- grid::grid.grabExpr(plot(x$frequency_spectrum, title = paste(label, "~ Frequency Spectrum")))
  wavelength_spectrum_grob <- grid::grid.grabExpr(plot(x$wavelength_spectrum, title = paste(label, "~ Wavelength Spectrum")))

  # Arrange the plots in the grid layout
  min_height_unit <- 50
  gridExtra::grid.arrange(
    frequency_spectrum_grob,
    composite_time, fundamental_time,
    composite_2d, fundamental_2d,
    composite_space, fundamental_space,
    wavelength_spectrum_grob,
    ncol = 2,
    layout_matrix = rbind(c(1, 1), c(2, 3), c(4, 5), c(6, 7), c(8,8)),
    heights = min_height_unit*c(1,1,2,1,1)  # Make the frequency plot span both columns with extra height
  )
}

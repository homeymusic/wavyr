#' @export
wave <- function(frequency_spectrum, wavelength_spectrum = NULL, phase = 0) {
  # Validate inputs
  if (!inherits(frequency_spectrum, "frequency_spectrum")) {
    stop("frequency_spectrum must be of class 'frequency_spectrum'")
  }

  # Automatically generate wavelength_spectrum if not provided
  if (is.null(wavelength_spectrum)) {
    wavelength_spectrum <- wavelength_spectrum(
      wavelength = SPEED_OF_SOUND / frequency_spectrum$frequency,
      amplitude = frequency_spectrum$amplitude
    )
  }

  if (!inherits(wavelength_spectrum, "wavelength_spectrum")) {
    stop("wavelength_spectrum must be of class 'wavelength_spectrum'")
  }

  if (!is.null(phase) && (!is.numeric(phase) || length(phase) != 1)) {
    stop("phase must be a single numeric value")
  }

  relative_f0 <- 1 / frequency_spectrum$relative_cycle_length
  relative_k0 <- 1 / wavelength_spectrum$relative_cycle_length

  coherence  <- (relative_f0 + relative_k0) / 2
  modulation <- (relative_f0 - relative_k0) / 2

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
  maximum_amplitude <- max(frequency_spectrum$amplitude) + max(wavelength_spectrum$amplitude)

  composite_amplitude <- function(x, t) {
    # Check that x and t are scalar values
    if (length(x) != 1 || length(t) != 1) {
      stop("x and t must be scalar values")
    }

    A0 <- maximum_amplitude

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

  maximum_fundamental_amplitude <- sum(frequency_spectrum$amplitude) + sum(wavelength_spectrum$amplitude)

  fundamental_amplitude <- function(x, t) {
    # Check that x and t are scalar values
    if (length(x) != 1 || length(t) != 1) {
      stop("x and t must be scalar values")
    }

    # Calculate the total amplitude as the sum of all amplitude contributions
    A0 <- maximum_fundamental_amplitude

    # Compute and return the amplitude at (x, t)
    A0 * cos(2 * pi * relative_f0 * t - 2 * pi * relative_k0 * x + phase)
  }

  fundamental_frequency_spectrum  = frequency_spectrum(
    frequency = frequency_spectrum$fundamental_frequency,
    amplitude = maximum_fundamental_amplitude
  )

  fundamental_wavelength_spectrum = wavelength_spectrum(
    wavelength = wavelength_spectrum$fundamental_wavelength,
    amplitude = maximum_fundamental_amplitude
  )

  # Return the structured object
  structure(
    list(
      frequency_spectrum = frequency_spectrum,
      wavelength_spectrum = wavelength_spectrum,
      fundamental_frequency_spectrum = fundamental_frequency_spectrum,
      fundamental_wavelength_spectrum = fundamental_wavelength_spectrum,
      phase = phase,
      indexed_spectra = indexed_spectra,
      fundamental_amplitude = fundamental_amplitude,
      composite_amplitude = composite_amplitude,
      relative_f0 = relative_f0,
      relative_k0 = relative_k0,
      coherence = coherence,
      modulation = modulation
    ),
    class = "wave"
  )
}

#' @export
`+.wave` <- function(wave1, wave2) {
  # Combine frequency spectra, summing amplitudes for identical components
  combined_frequency_spectrum <- combine_spectra(
    wave1$frequency_spectrum,
    wave2$frequency_spectrum,
    tolerance = FLOATING_POINT_TOLERANCE
  )

  # Combine wavelength spectra, summing amplitudes for identical components
  combined_wavelength_spectrum <- combine_spectra(
    wave1$wavelength_spectrum,
    wave2$wavelength_spectrum,
    tolerance = FLOATING_POINT_TOLERANCE
  )

  # Determine combined phase (average, or any other rule you choose)
  combined_phase <- (wave1$phase + wave2$phase) / 2

  # Create the new combined wave object
  combined_wave <- wave(
    frequency_spectrum = combined_frequency_spectrum,
    wavelength_spectrum = combined_wavelength_spectrum,
    phase = combined_phase
  )

  return(combined_wave)
}

#' @export
plot.wave <- function(x, label = '',
                          space_time_range = 25,
                          resolution = 99,
                          line_plot_resolution = 99,
                          wavelength_spectrum_grob = NULL, ...) {

  f0 <- x$frequency_spectrum$fundamental_frequency
  T0 <- 1 / x$frequency_spectrum$fundamental_frequency
  l0 <- x$wavelength_spectrum$fundamental_wavelength
  k0 <- 1 / x$wavelength_spectrum$fundamental_wavelength

  time_relative_cycle_length <- x$frequency_spectrum$relative_cycle_length
  space_relative_cycle_length <- x$wavelength_spectrum$relative_cycle_length

  tonality <- if (time_relative_cycle_length > space_relative_cycle_length) {
    'minor'
  } else if (time_relative_cycle_length == space_relative_cycle_length) {
    'neutral'
  } else {
    'major'
  }

  color_set <- saturation_colors_homey[[tonality]]

  # Calculate time and space ranges
  max_time <- space_time_range / time_relative_cycle_length * (1 / f0)
  max_space <- space_time_range / space_relative_cycle_length * (1 / k0)

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
      title = bquote(.(label) ~ "Composite"),
      subtitle = bquote(
        "Wavelengths:" ~ .(length(x$wavelength_spectrum$wavelength)) ~
          "~ Frequencies:" ~ .(length(x$frequency_spectrum$frequency))
      )
    ) +
    ggplot2::coord_fixed(ratio = 1) +
    theme_homey()

  # Time-only Composite with computed axis limits
  time_only_composite <- data.frame(
    time = time_values,
    amplitude = sapply(time_values, function(t) x$composite_amplitude(0, t))
  )
  composite_time <- ggplot2::ggplot(time_only_composite, ggplot2::aes(x = time, y = amplitude)) +
    ggplot2::geom_line(color = colors_homey$major) +
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
    ggplot2::geom_line(color = colors_homey$minor) +
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
      title = bquote(.(label) ~ "Fundamental"),
      subtitle = bquote(f[0]: ~ .(formatC(f0, format = "f", digits = 2)) ~ "Hz" ~ k[0]: ~ .(formatC(k0, format = "f", digits = 2)) ~ m^-1)
    ) +
    ggplot2::coord_fixed(ratio = 1) +
    theme_homey()

  # Time-only Fundamental with computed axis limits
  time_only_fundamental <- data.frame(
    time = time_values,
    amplitude = sapply(time_values, function(t) x$fundamental_amplitude(0, t))
  )
  fundamental_time <- ggplot2::ggplot(time_only_fundamental, ggplot2::aes(x = time, y = amplitude)) +
    ggplot2::geom_line(color = colors_homey$major) +
    ggplot2::scale_x_continuous(name = "Time (s)", limits = c(0, space_time_range), labels = function(x) sprintf("%.3f", x * max_time / space_time_range)) +
    ggplot2::labs(
      title = bquote(.(label) ~ "Fundamental Time Slice"),
      subtitle = bquote(T[0]: ~ .(formatC(T0, format = "f", digits = 4)) ~ "s" ~ f[0]: ~ .(formatC(f0, format = "f", digits = 2)) ~ "Hz")
    ) +
    ggplot2::scale_y_continuous(name = "") +
    theme_homey()

  # Space-only Fundamental with computed axis limits
  space_only_fundamental <- data.frame(
    space = space_values,
    amplitude = sapply(space_values, function(s) x$fundamental_amplitude(s, 0))
  )
  fundamental_space <- ggplot2::ggplot(space_only_fundamental, ggplot2::aes(x = space, y = amplitude)) +
    ggplot2::geom_line(color = colors_homey$minor) +
    ggplot2::scale_x_continuous(name = "Space (m)", limits = c(0, space_time_range), labels = function(y) sprintf("%.3f", y * max_space / space_time_range)) +
    ggplot2::labs(
      title = bquote(.(label) ~ "Fundamental Space Slice"),
      subtitle = bquote(lambda[0]: ~ .(formatC(l0, format = "f", digits = 2)) ~ "m " ~ k[0]: ~ .(formatC(k0, format = "f", digits = 4)) ~ m^-1)
    ) +
    ggplot2::scale_y_continuous(name = "") +
    theme_homey()

  frequency_spectrum_grob <- grid::grid.grabExpr(plot(x$frequency_spectrum,
                                                      rectangles = c(x$frequency_spectrum$frequency %>% min()),
                                                      title = paste(label, "~ Frequency Spectrum")))
  if (is.null(wavelength_spectrum_grob)) {
    wavelength_spectrum_grob <- grid::grid.grabExpr(
      plot(x$wavelength_spectrum,
           rectangles = c(SPEED_OF_SOUND / (x$frequency_spectrum$frequency %>% min())),
           title = paste(label, "~ Wavelength Spectrum"))
    )
  }

  fundamental_frequency_spectrum_grob <- grid::grid.grabExpr(plot(x$fundamental_frequency_spectrum, title = paste(label, "~ Fundamental Frequency Spectrum")))
  fundamental_wavelength_spectrum_grob <- grid::grid.grabExpr(plot(x$fundamental_wavelength_spectrum, title = paste(label, "~ Fundamental Wavelength Spectrum")))

  # Define the main title and subtitle as two separate labels
  main_title <- paste(label)
  subtitle <- paste(
    sprintf("Space Cyclicity: %.2f%%", x$relative_k0 * 100),
    sprintf("Time Cyclicity: %.2f%%", x$relative_f0 * 100),
    sprintf("Coherence: %.2f%%", x$coherence * 100),
    sprintf("Modulation: %.2f%%", x$modulation * 100),
    sep = "  "
  )

  # Create the title layout with reduced spacing
  top_title <- cowplot::ggdraw() +
    cowplot::draw_label(
      main_title,
      fontface = 'bold',
      size = 14,
      hjust = 0.5,
      color = colors_homey$foreground,
      y = 0.7  # Position main title higher
    ) +
    cowplot::draw_label(
      subtitle,
      fontface = 'plain',
      size = 10,  # Smaller size for subtitle
      hjust = 0.5,
      color = colors_homey$foreground,
      y = 0.4  # Position subtitle closer to the title
    )

  # Main layout with additional nested grids to align plots vertically
  final_plot <- cowplot::plot_grid(
    top_title,  # Title on top

    # Main grid with nested layouts
    cowplot::plot_grid(
      # Row 1 and 2: Rectangular plots with composite_2d spanning two rows in the third column
      cowplot::plot_grid(

        # Nested column 1: composite_space stacked above wavelength_spectrum_grob
        cowplot::plot_grid(x$wavelength_spectrum %>% space_signal() %>% plot(
          title='Space Series'
        ),
                           wavelength_spectrum_grob, ncol = 1, rel_heights = c(1, 1)),

        # Nested column 2: composite_time stacked above frequency_spectrum_grob
        cowplot::plot_grid(x$frequency_spectrum %>% time_signal() %>% plot(
          title='Time Series'
        ),
                           frequency_spectrum_grob, ncol = 1, rel_heights = c(1, 1)),

        # Column 3: composite_2d spanning two rows
        composite_2d,

        ncol = 3,
        rel_widths = c(1, 1, 1)
      ),

      # Row 3 and 4: Rectangular plots with fundamental_2d spanning two rows in the third column
      cowplot::plot_grid(
        # Nested column 1: fundamental_space stacked above fundamental_wavelength_spectrum_grob
        cowplot::plot_grid(fundamental_space, fundamental_wavelength_spectrum_grob, ncol = 1, rel_heights = c(1, 1)),

        # Nested column 2: fundamental_time stacked above fundamental_frequency_spectrum_grob
        cowplot::plot_grid(fundamental_time, fundamental_frequency_spectrum_grob, ncol = 1, rel_heights = c(1, 1)),

        # Column 3: fundamental_2d spanning two rows
        fundamental_2d,

        ncol = 3,
        rel_widths = c(1, 1, 1)
      ),

      # Set main layout with consistent row heights
      ncol = 1,
      rel_heights = c(2, 2)
    ),

    # Adjust relative heights for title and main content to reduce padding
    ncol = 1,
    rel_heights = c(0.08, 1)  # Reduce title height relative to main grid to limit white space
  )

  # Print or return the final plot for display
  print(final_plot)
}

#' Create a general waveform
#'
#' @param frequency_spectrum An object of class "frequency_spectrum" containing frequencies and amplitudes.
#' @param wavelength_spectrum Optional: An object of class "wavelength_spectrum" for custom wavelength components.
#' @param phase Optional: A numeric value representing the phase of the waveform.
#'
#' @return An object of class "waveform" containing the frequency spectrum, wavelength spectrum, phase, and indexed_spectra.
#' @export
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

      # Calculate the equivalent frequency
      equivalent_frequency <- SPEED_OF_SOUND / wavelength

      # Find any matching frequency within tolerance
      matched_indices <- which(abs(frequency_spectrum$frequency - equivalent_frequency) < 1e-6)

      # Construct the tibble for matched or unmatched cases
      tibble::tibble(
        frequency = if (length(matched_indices) > 0) frequency_spectrum$frequency[matched_indices][1] else NA,
        frequency_amplitude = if (length(matched_indices) > 0) {sum(frequency_spectrum$amplitude[matched_indices])} else NA,
        frequency_cycle_length = if (length(matched_indices) > 0) frequency_spectrum$cycle_length[matched_indices][1] else NA,
        wavelength = wavelength,
        wavelength_amplitude = wavelength_amplitude,
        wavelength_cycle_length = wavelength_cycle_length
      )
    }
  ) %>% dplyr::arrange(dplyr::desc(wavelength))

  composite_amplitude <- function(x, t) {
    # Check that x and t are scalar values
    if (length(x) != 1 || length(t) != 1) {
      stop("x and t must be scalar values")
    }

    # Calculate the composite amplitude as a sum of contributions
    sum(
      purrr::pmap_dbl(indexed_spectra, function(frequency,
                                                frequency_amplitude,
                                                frequency_cycle_length,
                                                wavelength,
                                                wavelength_amplitude,
                                                wavelength_cycle_length) {

        # Calculate amplitude component
        An <- ifelse(is.na(frequency_amplitude), 0, frequency_amplitude) +
          ifelse(is.na(wavelength_amplitude), 0, wavelength_amplitude)

        # Calculate relative frequencies
        relative_fn <- ifelse(is.na(frequency_cycle_length), 0, 1 / frequency_cycle_length)
        relative_kn <- ifelse(is.na(wavelength_cycle_length), 0, 1 / wavelength_cycle_length)

        # Return the amplitude contribution for this component as a traveling wave
        An * cos(2 * pi * (relative_fn * t - relative_kn * x) + phase)
      })
    )
  }

  # Define the fundamental amplitude function
  fundamental_amplitude <- function(x, t) {
    # Check that x and t are scalar values
    if (length(x) != 1 || length(t) != 1) {
      stop("x and t must be scalar values")
    }

    # Calculate relative frequency and wavenumber
    relative_f0 <- 1 / frequency_spectrum$fundamental_cycle_length
    relative_k0 <- 1 / wavelength_spectrum$fundamental_cycle_length

    # Calculate maximum amplitude
    A0 <- max(frequency_spectrum$amplitude) + max(wavelength_spectrum$amplitude)

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

midi_to_freq <- function(midi) {
  440 * 2^((midi - 69) / 12)
}

SPEED_OF_SOUND = midi_to_freq(65)

colors_homey <- list(
  'background'        = '#664433',
  'highlight'         = '#C18160',
  'foreground'        = '#291B14',
  'subtle_foreground' = '#7F745A',
  'minor'             = '#8AC5FF',
  'minor_dark'        = '#6894BF',
  'neutral'           = '#F3DDAB',
  'major'             = '#FFB000',
  'major_dark'        = '#BF8400',
  'light_neutral'     = '#FFF6E2',
  'fundamental'       = '#FF5500',
  'green'             = '#74DE7E',
  'gray'              = '#C0C0C0'
)

saturation_colors_homey <- list(
  major = list(
    hi = '#FF9700',
    lo = '#FFE0B2'
  ),
  neutral = list(
    hi = '#FF5500',
    lo = '#FFCCB2'
  ),
  minor = list(
    hi = '#0084EC',
    lo = '#A5CDEC'
  )
)

color_factor_homey <- function(x,column_name) {
  cut(x[[column_name]],c(-Inf,-1e-6,1e-6,Inf),labels=c("minor","neutral","major"))
}
color_values_homey <- function() {
  c("minor"=colors_homey$minor,
    "neutral"=colors_homey$fundamental,
    "major"=colors_homey$major,
    'behavioral'=colors_homey$neutral)
}
space_time_colors <- function() {
  c('space'=colors_homey$minor,
    'time'=colors_homey$major,
    'behavioral'=colors_homey$neutral)
}
theme_homey <- function(aspect.ratio = NULL) {
  font <- "Helvetica"   # Assign font family up front

  ggplot2::theme_minimal() %+replace% ggplot2::theme(
    plot.title = ggplot2::element_text(color = colors_homey$foreground),
    axis.title = ggplot2::element_text(color = colors_homey$foreground),
    axis.text = ggplot2::element_text(color = colors_homey$foreground),
    axis.ticks = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = colors_homey$neutral),
    panel.background = ggplot2::element_rect(fill = colors_homey$background),
    panel.grid.major = ggplot2::element_line(color = colors_homey$foreground, linewidth = 0.2),
    panel.grid.minor = ggplot2::element_line(color = colors_homey$foreground, linewidth = 0.05, linetype = "dashed"),
    legend.background = ggplot2::element_rect(fill = colors_homey$light_neutral),
    legend.key = ggplot2::element_rect(fill = colors_homey$background, color = NA),
    legend.position = 'bottom',
    aspect.ratio = aspect.ratio
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

  # Define the theme with cream and brown backgrounds
  custom_theme <- ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = colors_homey$neutral, color = colors_homey$foreground),
    panel.background = ggplot2::element_rect(fill = colors_homey$background),
    panel.grid.major = ggplot2::element_line(color = colors_homey$foreground, linewidth = 0.2),
    panel.grid.minor = ggplot2::element_line(color = colors_homey$foreground, linewidth = 0.05, linetype = "dashed"),
    axis.text = ggplot2::element_text(color = colors_homey$foreground),
    axis.title = ggplot2::element_text(color = colors_homey$foreground),
    plot.title = ggplot2::element_text(color = colors_homey$foreground, face = "bold"),
    legend.background = ggplot2::element_rect(fill = colors_homey$light_neutral),
    legend.key = ggplot2::element_rect(fill = colors_homey$background, color = NA)
  )

  # 2D Composite Amplitude
  grid$composite_amplitude <- mapply(
    function(space, time) x$composite_amplitude(space, time),
    grid$space, grid$time
  )
  composite_2d <- ggplot2::ggplot(grid, ggplot2::aes(x = time, y = space, fill = composite_amplitude)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(low = color_set$lo, high = color_set$hi) +
    ggplot2::scale_x_continuous(name = "Time (s)", labels = function(x) sprintf("%.3f", x * max_time / space_time_range), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(name = "Space (m)", labels = function(y) sprintf("%.3f", y * max_space / space_time_range), expand = c(0, 0)) +
    ggplot2::labs(title = paste(label, "Composite 2D")) +
    ggplot2::coord_fixed(ratio = 1) +
    custom_theme

  # Time-only Composite with computed axis limits
  time_only_composite <- data.frame(
    time = time_values,
    amplitude = sapply(time_values, function(t) x$composite_amplitude(0, t))
  )
  composite_time <- ggplot2::ggplot(time_only_composite, ggplot2::aes(x = time, y = amplitude)) +
    ggplot2::geom_line(color = color_set$hi) +
    ggplot2::scale_x_continuous(name = "Time (s)", limits = c(0, space_time_range), labels = function(x) sprintf("%.3f", x * max_time / space_time_range)) +
    ggplot2::labs(title = paste(label, "Composite Time Slice")) +
    custom_theme

  # Space-only Composite with computed axis limits
  space_only_composite <- data.frame(
    space = space_values,
    amplitude = sapply(space_values, function(s) x$composite_amplitude(s, 0))
  )
  composite_space <- ggplot2::ggplot(space_only_composite, ggplot2::aes(x = space, y = amplitude)) +
    ggplot2::geom_line(color = color_set$hi) +
    ggplot2::scale_x_continuous(name = "Space (m)", limits = c(0, space_time_range), labels = function(y) sprintf("%.3f", y * max_space / space_time_range)) +
    ggplot2::labs(title = paste(label, "Composite Space Slice")) +
    custom_theme

  # 2D Fundamental Amplitude
  grid$fundamental_amplitude <- mapply(
    function(space, time) x$fundamental_amplitude(space, time),
    grid$space, grid$time
  )
  fundamental_2d <- ggplot2::ggplot(grid, ggplot2::aes(x = time, y = space, fill = fundamental_amplitude)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(low = color_set$lo, high = color_set$hi) +
    ggplot2::scale_x_continuous(name = "Time (s)", labels = function(x) sprintf("%.3f", x * max_time / space_time_range), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(name = "Space (m)", labels = function(y) sprintf("%.3f", y * max_space / space_time_range), expand = c(0, 0)) +
    ggplot2::labs(title = paste(label, "Fundamental 2D")) +
    ggplot2::coord_fixed(ratio = 1) +
    custom_theme

  # Time-only Fundamental with computed axis limits
  time_only_fundamental <- data.frame(
    time = time_values,
    amplitude = sapply(time_values, function(t) x$fundamental_amplitude(0, t))
  )
  fundamental_time <- ggplot2::ggplot(time_only_fundamental, ggplot2::aes(x = time, y = amplitude)) +
    ggplot2::geom_line(color = color_set$hi) +
    ggplot2::scale_x_continuous(name = "Time (s)", limits = c(0, space_time_range), labels = function(x) sprintf("%.3f", x * max_time / space_time_range)) +
    ggplot2::labs(title = paste(label, "Fundamental Time Slice")) +
    custom_theme

  # Space-only Fundamental with computed axis limits
  space_only_fundamental <- data.frame(
    space = space_values,
    amplitude = sapply(space_values, function(s) x$fundamental_amplitude(s, 0))
  )
  fundamental_space <- ggplot2::ggplot(space_only_fundamental, ggplot2::aes(x = space, y = amplitude)) +
    ggplot2::geom_line(color = color_set$hi) +
    ggplot2::scale_x_continuous(name = "Space (m)", limits = c(0, space_time_range), labels = function(y) sprintf("%.3f", y * max_space / space_time_range)) +
    ggplot2::labs(title = paste(label, "Fundamental Space Slice")) +
    custom_theme

  # Arrange in grid with label at the top
  gridExtra::grid.arrange(
    composite_time, fundamental_time,
    composite_2d, fundamental_2d,
    composite_space, fundamental_space,
    ncol = 2,
    top = grid::textGrob(label, gp = grid::gpar(fontsize = 16, fontface = "bold"))
  )
}

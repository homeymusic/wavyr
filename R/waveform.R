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

  indexed_spectra <- purrr::map2_dfr(
    wavelength_spectrum$wavelength,
    wavelength_spectrum$amplitude,
    function(wavelength, wavelength_amplitude) {

      # Calculate the equivalent frequency
      equivalent_frequency <- SPEED_OF_SOUND / wavelength

      # Find any matching frequency within tolerance
      matched_indices <- which(abs(frequency_spectrum$frequency - equivalent_frequency) < 1e-6)

      # Construct the tibble for matched or unmatched cases
      tibble::tibble(
        frequency = if (length(matched_indices) > 0) frequency_spectrum$frequency[matched_indices][1] else NA,
        frequency_amplitude = if (length(matched_indices) > 0) {
          sum(frequency_spectrum$amplitude[matched_indices])
        } else NA,
        wavelength = wavelength,
        wavelength_amplitude = wavelength_amplitude
      )
    }
  ) %>% dplyr::arrange(dplyr::desc(wavelength))

  # Define the fundamental amplitude function
  fundamental_amplitude <- function(x, t) {
    f0 <- frequency_spectrum$fundamental_frequency
    l0 <- wavelength_spectrum$fundamental_wavelength
    A0 <- max(frequency_spectrum$amplitude) +
      max(wavelength_spectrum$amplitude)
    A0 * cos((2 * pi / l0) * x - (2 * pi * f0) * t + phase)
  }

  composite_amplitude <- function(x, t) {
    sum(
      purrr::pmap_dbl(indexed_spectra, function(frequency,
                                                frequency_amplitude,
                                                wavelength,
                                                wavelength_amplitude) {

        # Handle NA values and compute angular frequencies and wavenumbers
        angular_f <- ifelse(is.na(frequency), 0, 2 * pi * frequency)
        angular_k <- ifelse(is.na(wavelength), 0, (2 * pi) / wavelength)

        # Handle NA values in amplitudes
        An <- ifelse(is.na(frequency_amplitude), 0, frequency_amplitude) +
          ifelse(is.na(wavelength_amplitude), 0, wavelength_amplitude)

        # Compute the amplitude contribution from this component
        An * cos(angular_k * x - angular_f * t + phase)
      })
    )
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

#' Plot a waveform in 2D with time and space using lattice
#'
#' @param x A `waveform` object containing independent frequency and wavelength spectra.
#' @param time_range A numeric vector of two elements specifying the time range for the plot.
#' @param space_range A numeric vector of two elements specifying the spatial range for the plot.
#' @param resolution A numeric value to set the resolution of the plot grid.
#' @param ... Additional graphical parameters for plotting.
#'
#' @export
plot.waveform <- function(x, time_range = c(0, 10), space_range = c(0, 10), resolution = 100, ...) {

  # Set up the grid of time and space
  time_points <- seq(time_range[1], time_range[2], length.out = resolution)
  space_points <- seq(space_range[1], space_range[2], length.out = resolution)

  # Adjust the time range based on space range and speed of sound
  speed_of_sound <- 343  # Example: Speed of sound in m/s
  scaled_time_range <- space_range / speed_of_sound  # Scaling the time range according to the space range

  # Expand grid for time and space points
  composite_waveform <- tidyr::expand_grid(Time = time_points, Space = space_points)

  # Calculate the waveform amplitudes using fundamental_amplitude and composite_amplitude
  composite_waveform <- composite_waveform %>%
    dplyr::mutate(
      Amplitude = purrr::map2_dbl(
        Time, Space,
        function(t, s) {
          # Use composite_amplitude for the total amplitude at each (x, t)
          x$composite_amplitude(s, t)
        }
      )
    )

  # Create the waveform plot using the composite amplitude
  composite_plot <- lattice::levelplot(
    Amplitude ~ Time * Space, data = composite_waveform,
    xlab = "Time", ylab = "Space",
    main = "Composite Waveform",
    scales = list(draw = FALSE),
    col.regions = gray.colors(100),
    aspect = "iso",
    colorkey = FALSE
  )

  # Create the fundamentals plot using fundamental_amplitude
  fundamental_waveform <- tidyr::expand_grid(Time = time_points, Space = space_points)
  fundamental_waveform$Amplitude <- purrr::map2_dbl(
    fundamental_waveform$Time, fundamental_waveform$Space,
    function(t, s) x$fundamental_amplitude(s, t)
  )

  # Create the fundamentals plot
  fundamental_plot <- lattice::levelplot(
    Amplitude ~ Time * Space, data = fundamental_waveform,
    xlab = "Time", ylab = "Space",
    main = "Fundamental Waveform",
    scales = list(draw = FALSE),
    col.regions = gray.colors(100),
    aspect = "iso",
    colorkey = FALSE
  )

  # Arrange the plots side by side
  gridExtra::grid.arrange(composite_plot, fundamental_plot, ncol = 2)
}

#' Create a general waveform
#'
#' @param frequency_spectrum An object of class "frequency_spectrum" containing frequencies and amplitudes.
#' @param wavelength_spectrum Optional: An object of class "wavelength_spectrum" for custom wavelength components.
#' @param phase Optional: A numeric value representing the phase of the waveform.
#'
#' @return An object of class "waveform" containing the frequency spectrum, wavelength spectrum, and phase.
#' @export
waveform <- function(frequency_spectrum, wavelength_spectrum = NULL, phase = 0) {
  # Validate inputs
  if (!inherits(frequency_spectrum, "frequency_spectrum")) {
    stop("frequency_spectrum must be of class 'frequency_spectrum'")
  }
  if (!is.null(wavelength_spectrum) && !inherits(wavelength_spectrum, "wavelength_spectrum")) {
    stop("wavelength_spectrum must be of class 'wavelength_spectrum'")
  }
  if (!is.null(phase) && (!is.numeric(phase) || length(phase) != 1)) {
    stop("phase must be a single numeric value")
  }

  # Return the structured object
  structure(
    list(
      frequency_spectrum = frequency_spectrum,
      wavelength_spectrum = wavelength_spectrum,
      phase = phase
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

  # Create a matrix to store amplitude values for the waveform
  amplitude_matrix <- matrix(0, nrow = resolution, ncol = resolution)

  # Calculate waveform using linear frequencies and wavelengths
  for (i in seq_along(x$frequency_spectrum$component)) {
    frequency <- x$frequency_spectrum$component[i]

    for (j in seq_along(x$wavelength_spectrum$component)) {
      wavelength <- x$wavelength_spectrum$component[j]

      # Calculate linear wave number and frequency
      angular_k <- (2 * pi) / wavelength
      angular_f <- 2 * pi * frequency

      # Generate the wave for this frequency and wavelength
      wave <- outer(
        time_points, space_points,
        function(t, s) {
          amplitude <- x$frequency_spectrum$amplitude[i]  # Use the amplitude of the current frequency
          amplitude * cos(angular_k * s - angular_f * t + x$phase)
        }
      )

      amplitude_matrix <- amplitude_matrix + wave
    }
  }

  # Create a data frame for the waveform plot
  waveform_df <- expand.grid(Time = time_points, Space = space_points)
  waveform_df$Amplitude <- as.vector(amplitude_matrix)

  # Create the waveform plot
  waveform_plot <- lattice::levelplot(
    Amplitude ~ Time * Space, data = waveform_df,
    xlab = "Time", ylab = "Space",
    main = "Full Waveform",
    scales = list(draw = FALSE),  # Remove axis ticks and labels
    col.regions = gray.colors(100),
    aspect = "iso",  # Set aspect ratio to 1
    colorkey=FALSE
  )

  # Create the fundamentals plot using the same time and space points
  # Assuming linear_k and linear_f can be calculated once for the fundamental plot
  fundamental_matrix <- outer(
    time_points, space_points,
    function(t, s) {
      linear_k <- 1 / x$wavelength_spectrum$cycle_length  # Use the first wavelength as an example
      linear_f <- 1 / x$frequency_spectrum$cycle_length    # Use the first frequency as an example
      amplitude <- 1  # Use constant amplitude for the fundamental plot
      amplitude * cos(linear_k * s - linear_f * t)
    }
  )

  # Create a data frame for the fundamentals plot
  fundamentals_df <- expand.grid(Time = time_points, Space = space_points)
  fundamentals_df$Amplitude <- as.vector(fundamental_matrix)

  # Create the fundamentals plot
  fundamentals_plot <- lattice::levelplot(
    Amplitude ~ Time * Space, data = fundamentals_df,
    xlab = "Time", ylab = "Space",
    main = "Fundamental Waveform",
    scales = list(draw = FALSE),  # Remove axis ticks and labels
    col.regions = gray.colors(100),
    aspect = "iso",  # Set aspect ratio to 1
    colorkey=FALSE
  )

  # Arrange the plots side by side
  gridExtra::grid.arrange(waveform_plot, fundamentals_plot, ncol = 2)
}

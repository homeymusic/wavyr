intervals <- list(
  # consonant intervals
  Major3 = 64,
  minor6 = 68,
  Major6 = 69,
  minor3 = 63,
  # perfect intervals
  Perfect5 = 67,
  Perfect4 = 65,
  Perfect1 = 60,
  Perfect8 = 72,
  # dissonant intervals
  tritone = 66,
  Major7 = 71,
  minor2 = 61,
  Major2 = 62,
  minor7 = 70
)
framed_intervals <- purrr::map(intervals, ~c(60, .x, 72))

triads <- list(
  # consonant intervals
  ionian   =  60 + c(4,7),
  phrygian =  72 - c(4,7)
)
framed_triads <- purrr::map(triads, ~c(60, .x, 72) %>% sort())

spectrum_for <- function(x, num_harmonics = 1, roll_off_dB = 1) {

  purrr::map2(midi_to_freq(x), rep(1, length(x)),
              function(freq, amp) {
                n  <- seq_len(num_harmonics)
                df <- data.frame(
                  idealized_frequency = freq * n,
                  amplitude = 1 * 10 ^ ( -roll_off_dB * log2(n) / 20)
                )
                df
              }) %>%
    dplyr::bind_rows()
}

frequency_spectrum_for <- function(x, num_harmonics = 1, roll_off_dB = 1) {
  spectrum_for(x, num_harmonics, roll_off_dB) %>% frequency_spectrum()
}

wave_for <- function(x, num_harmonics = 1, roll_off_dB = 1) {
  frequency_spectrum_for(x,
                         num_harmonics = num_harmonics,
                         roll_off_dB = roll_off_dB) %>%
    wave()
}

plot_error_histogram <- function(errors) {
  if (all(errors == 0)) {
    total_count <- length(errors)

    # Generate approximately 5 evenly spaced odd labels
    y_ticks <- seq(1, total_count, length.out = 5)
    y_ticks <- round(y_ticks)  # Round to nearest integers
    y_ticks <- ifelse(y_ticks %% 2 == 0, y_ticks + 1, y_ticks)  # Ensure odd labels

    hist(
      errors,
      breaks = c(-0.5, 0.5),  # Single bin centered at 0
      main = "Error Histogram (All Zeros)",
      xlab = "Errors",
      ylab = "Frequency",
      col = "darkgray",
      border = "black",
      ylim = c(0, total_count),  # Ensure the bar height matches total count
      xaxt = "n",  # Suppress x-axis
      yaxt = "n"   # Suppress y-axis
    )
    axis(1, at = 0, labels = "0")  # Custom x-axis with only 0
    axis(2, at = y_ticks, labels = y_ticks)  # Custom y-axis with odd-numbered labels
    return()
  }

  # General case
  max_error <- max(abs(errors))  # Get the maximum absolute error
  bin_width <- 2 * max_error / 20  # Divide the range into 20 bins
  # Adjust breaks to ensure 0 is at the center of a bin
  breaks <- seq(-max_error - bin_width / 2, max_error + bin_width / 2, by = bin_width)

  hist(
    errors,
    breaks = breaks,
    main = "Error Histogram",
    xlab = "Errors",
    ylab = "Frequency",
    col = "darkgray",
    border = "black"
  )
}

plot_matrix <- function(matrix, fft_shift = T, magnitude = T, log_scaling = F) {
  # Convert complex values to magnitudes
  magnitude_matrix = matrix

  if (fft_shift) {
    magnitude_matrix <- fft_shift(magnitude_matrix)
  }

  if (magnitude) {
    magnitude_matrix <- Mod(magnitude_matrix)
  } else {
    magnitude_matrix <- Re(magnitude_matrix)
  }

  if (log_scaling) {
    magnitude_matrix <- log1p(magnitude_matrix)
  }

    # Plot the matrix as an image
    par(mar = c(0, 0, 0, 0))  # Remove margins to ensure no additional space
    image(
      1:ncol(magnitude_matrix),
      1:nrow(magnitude_matrix),
      t(magnitude_matrix[nrow(magnitude_matrix):1, ]),  # Flip y-axis for correct orientation
      col = gray.colors(256, start = 0, end = 1),  # Grayscale color scheme
      asp = 1,  # Ensure square cells
      axes = FALSE,  # Turn off axes
      xlab = "",  # Remove x-axis label
      ylab = "",  # Remove y-axis label
      useRaster = TRUE  # Use rasterized rendering for smooth output
    )
}

fft_shift <- function(input_matrix) {

  rows <- dim(input_matrix)[1]
  cols <- dim(input_matrix)[2]

  swap_up_down <- function(input_matrix) {
    rows_half <- ceiling(rows/2)
    return(rbind(input_matrix[((rows_half+1):rows), (1:cols)], input_matrix[(1:rows_half), (1:cols)]))
  }

  swap_left_right <- function(input_matrix) {
    cols_half <- ceiling(cols/2)
    return(cbind(input_matrix[1:rows, ((cols_half+1):cols)], input_matrix[1:rows, 1:cols_half]))
  }

  input_matrix <- swap_up_down(input_matrix)
  return(swap_left_right(input_matrix))
}

# Helper function to load and preprocess an image
load_and_preprocess_image <- function(image_file_path) {
  original <- imager::load.image(image_file_path)
  grayscale_image <- imager::grayscale(original)
  list(
    original_dim = dim(original),
    grayscale_matrix = as.matrix(grayscale_image)
  )
}

normalize_matrix_to_zero_corners <- function(mat) {
  s = mat %>% fft_shift()
  round((s[1,1] - s) * 1e10) / 1e10
}

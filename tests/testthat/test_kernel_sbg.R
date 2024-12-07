source(testthat::test_path("helper.R"))

# Helper function to load and preprocess an image
load_and_preprocess_image <- function(image_file_path) {
  original <- imager::load.image(image_file_path)
  grayscale_image <- imager::grayscale(original)
  list(
    original_dim = dim(original),
    grayscale_matrix = as.matrix(grayscale_image)
  )
}

# Helper function to test and visualize a sbg kernel
test_sbg_kernel <- function(label_prefix, kernel, suffix = "", fft_shift = FALSE, magnitude = FALSE, log_scaling = FALSE) {
  label <- paste(label_prefix, suffix)
  vdiffr::expect_doppelganger(
    label,
    plot_matrix(kernel, fft_shift = fft_shift, magnitude = magnitude, log_scaling = log_scaling)
  )
}

# Helper function to apply sbg filter and visualize the result
test_sbg_filter <- function(label_prefix, grayscale_matrix, kernel, suffix = "") {
  response <- imager::convolve(imager::as.cimg(grayscale_matrix), imager::as.cimg(Re(kernel))) +
    1i * imager::convolve(imager::as.cimg(grayscale_matrix), imager::as.cimg(Im(kernel)))
  filtered_image <- imager::as.cimg(Mod(response), dim = dim(grayscale_matrix))
  label <- paste(label_prefix, suffix)
  vdiffr::expect_doppelganger(
    label,
    plot(filtered_image, axes = FALSE)
  )
}

# Helper function to compute and visualize sbg spectrum
test_sbg_spectrum <- function(label_prefix, kernel, suffix = "") {
  spectrum <- fftwtools::fftw2d(kernel)
  label <- paste(label_prefix, suffix)
  vdiffr::expect_doppelganger(label, plot_matrix(spectrum))
  spectrum
}

test_that('sbg filters with varied parameters', {
  params <- list(
    kernel_sizes = c(31),
    gammas = c(0.8),
    etas = c(0.8),
    frequencies = c(0.1),
    phases = c(0),
    orientations = c(0, 1/4, 1/2, 3/4) * pi
  )

  image_data <- load_and_preprocess_image(test_path("images", "Lenna.png"))
  grayscale_matrix <- image_data$grayscale_matrix

  for (kernel_size in params$kernel_sizes) {
    for (gamma in params$gammas) {
      for (eta in params$etas) {
        for (frequency in params$frequencies) {
          for (phase in params$phases) {
            for (orientation in params$orientations) {
              suffix <- paste(kernel_size, gamma, eta, frequency, phase, orientation / pi)

              sbg_kernel <- kernel_sbg(kernel_size, gamma, eta, orientation, frequency, phase)
              test_sbg_kernel('orig kernel', sbg_kernel, suffix)

              test_sbg_filter('Lenna sbg Filtered', grayscale_matrix, sbg_kernel, suffix)

              spectrum <- test_sbg_spectrum('kernel spectrum', sbg_kernel, suffix)

              reconstructed <- fftwtools::fftw2d(spectrum, inverse = 1)
              test_sbg_kernel('reconstructed kernel', reconstructed, suffix)
            }
          }
        }
      }
    }
  }
})

test_that('sbg filters with combined parameters', {
  kernel_size <- 31
  gamma <- 0.8
  eta <- 0.8
  frequency <- 0.1
  phase <- 0
  orientations <- c(0, 1/4, 1/2, 3/4) * pi

  combined_kernel <- matrix(0 + 0i, nrow = kernel_size, ncol = kernel_size)
  for (orientation in orientations) {
    combined_kernel <- combined_kernel + kernel_sbg(kernel_size, gamma, eta, orientation, frequency, phase)
  }

  test_sbg_kernel('combined kernel', combined_kernel)

  lenna_data <- load_and_preprocess_image(test_path("images", "Lenna.png"))
  test_sbg_filter('Combined Lenna sbg Filtered', lenna_data$grayscale_matrix, combined_kernel)

  madukes_data <- load_and_preprocess_image(test_path("images", "MaDukes.png"))
  test_sbg_filter('Combined Ma Dukes sbg Filtered', madukes_data$grayscale_matrix, combined_kernel)

  combined_spectrum <- test_sbg_spectrum('combined kernel spectrum', combined_kernel)

  reconstructed_combined <- fftwtools::fftw2d(combined_spectrum, inverse = 1)
  test_sbg_kernel('combined reconstructed kernel', reconstructed_combined)
})

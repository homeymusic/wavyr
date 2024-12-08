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

# Helper function to test and visualize a Gabor kernel
test_gabor_kernel <- function(label_prefix, kernel, suffix = "", fft_shift = FALSE, magnitude = FALSE, log_scaling = FALSE) {
  label <- paste(label_prefix, suffix)
  vdiffr::expect_doppelganger(
    label,
    plot_matrix(kernel, fft_shift = fft_shift, magnitude = magnitude, log_scaling = log_scaling)
  )
}

# Helper function to apply Gabor filter and visualize the result
test_gabor_filter <- function(label_prefix, grayscale_matrix, kernel, suffix = "") {
  response <- convolution_with_kernel(grayscale_matrix, kernel)
  filtered_image <- imager::as.cimg(Mod(response), dim = dim(grayscale_matrix))
  label <- paste(label_prefix, suffix)
  vdiffr::expect_doppelganger(
    label,
    plot(filtered_image, axes = FALSE)
  )
}

# Helper function to compute and visualize Gabor spectrum
test_gabor_spectrum <- function(label_prefix, kernel, suffix = "") {
  spectrum <- fftwtools::fftw2d(kernel)
  label <- paste(label_prefix, suffix)
  vdiffr::expect_doppelganger(label, plot_matrix(spectrum))
  spectrum
}

test_that('gabor filters with varied parameters', {
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

              gabor_kernel <- kernel_gabor(kernel_size, gamma, eta, orientation, frequency, phase)
              test_gabor_kernel('orig kernel', gabor_kernel, suffix)

              test_gabor_filter('Lenna Gabor Filtered', grayscale_matrix, gabor_kernel, suffix)

              spectrum <- test_gabor_spectrum('kernel spectrum', gabor_kernel, suffix)

              reconstructed <- fftwtools::fftw2d(spectrum, inverse = 1)
              test_gabor_kernel('reconstructed kernel', reconstructed, suffix)
            }
          }
        }
      }
    }
  }
})

test_that('gabor filters with combined parameters', {
  kernel_size <- 31
  gamma <- 0.8
  eta <- 0.8
  frequency <- 0.1
  phase <- 0
  orientations <- c(0, 1/4, 1/2, 3/4) * pi

  combined_kernel <- matrix(0 + 0i, nrow = kernel_size, ncol = kernel_size)
  for (orientation in orientations) {
    combined_kernel <- combined_kernel + kernel_gabor(kernel_size, gamma, eta, orientation, frequency, phase)
  }

  test_gabor_kernel('combined kernel', combined_kernel)

  lenna_data <- load_and_preprocess_image(test_path("images", "Lenna.png"))
  test_gabor_filter('Combined Lenna Gabor Filtered', lenna_data$grayscale_matrix, combined_kernel)

  madukes_data <- load_and_preprocess_image(test_path("images", "MaDukes.png"))
  test_gabor_filter('Combined Ma Dukes Gabor Filtered', madukes_data$grayscale_matrix, combined_kernel)

  combined_spectrum <- test_gabor_spectrum('combined kernel spectrum', combined_kernel)

  reconstructed_combined <- fftwtools::fftw2d(combined_spectrum, inverse = 1)
  test_gabor_kernel('combined reconstructed kernel', reconstructed_combined)
})

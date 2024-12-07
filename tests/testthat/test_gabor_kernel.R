source(testthat::test_path("helper.R"))

test_that('gabor filters with varied parameters', {

  kernel_sizes <- c(31)  # Try different kernel sizes
  gammas <- c(0.8)  # Aspect ratios in x
  etas <- c(0.8)  # Aspect ratios in y
  frequencies <- c(0.1)  # Different sinusoidal densities
  phases <- c(0)  # Phase offsets
  orientations <- c(0, 1/4, 1/2, 3/4) * pi

  image_file_path <- test_path("images", "Lenna.png")
  expected_original <- imager::load.image(image_file_path)
  expected_original_dim <- dim(expected_original)
  expected_grayscale_image = imager::grayscale(expected_original)
  expected_grayscale_image_matrix = as.matrix(expected_grayscale_image)

  for (kernel_size in kernel_sizes) {
    for (gamma in gammas) {
      for (eta in etas) {
        for (f in frequencies) {
          for (psi in phases) {
            for (orientation in orientations) {

              label = paste('orig kernel', kernel_size, gamma, eta, f, psi, orientation / pi)
              gabor_kernel <- create_gabor_kernel(kernel_size, gamma, eta, orientation, f, psi)
              vdiffr::expect_doppelganger(
                label,
                plot_matrix(gabor_kernel, fft_shift = F, magnitude = F, log_scaling = F)
              )

              label = paste('Lenna Gabor Filtered', kernel_size, gamma, eta, f, psi, orientation / pi)
              gabor_filtered_image = apply_gabor_filter(expected_grayscale_image_matrix,
                                                        orientation,
                                                        f,
                                                        kernel_size)
              vdiffr::expect_doppelganger(
                label,
                plot(gabor_filtered_image, axes = F)
              )

              label = paste('kernel spectrum', kernel_size, gamma, eta, f, psi, orientation / pi)
              gabor_spectrum = fftwtools::fftw2d(gabor_kernel)
              vdiffr::expect_doppelganger(
                label,
                plot_matrix(gabor_spectrum)
              )

              label = paste('reconstructed kernel', kernel_size, gamma, eta, f, psi, orientation / pi)
              gabor_signal <- fftwtools::fftw2d(gabor_spectrum, inverse = 1)
              vdiffr::expect_doppelganger(
                label,
                plot_matrix(gabor_signal, fft_shift = F, magnitude = F, log_scaling = F)
              )

            }
          }
        }
      }
    }
  }
})

test_that('gabor filters with combined parameters', {

  kernel_size = 31
  gamma = 0.8
  eta = 0.8
  frequency = 0.1
  phase = 0
  orientations <- c(0, 1/4, 1/2, 3/4) * pi

  image_file_path <- test_path("images", "Lenna.png")
  expected_original <- imager::load.image(image_file_path)
  expected_original_dim <- dim(expected_original)
  expected_grayscale_image = imager::grayscale(expected_original)
  expected_grayscale_image_matrix = as.matrix(expected_grayscale_image)

  combined_gabor_kernel = matrix(0 + 0i, nrow = kernel_size, ncol = kernel_size)

  for (orientation in orientations) {

    gabor_kernel <- create_gabor_kernel(kernel_size, gamma, eta, orientation, frequency, psi)
    combined_gabor_kernel =  combined_gabor_kernel + gabor_kernel
  }

  label = paste('combined kernel')
  vdiffr::expect_doppelganger(
    label,
    plot_matrix(combined_gabor_kernel, fft_shift = F, magnitude = F, log_scaling = F)
  )

  label = paste('Combined Lenna Gabor Filtered')
  # Perform convolution with real and imaginary components
  response <- imager::convolve(imager::as.cimg(expected_grayscale_image_matrix), imager::as.cimg(Re(combined_gabor_kernel))) +
    1i * imager::convolve(imager::as.cimg(expected_grayscale_image_matrix), imager::as.cimg(Im(combined_gabor_kernel)))

  # Return the magnitude of the response as an image
  gabor_filtered_image = imager::as.cimg(Mod(response), dim = dim(grayscale_matrix))

  vdiffr::expect_doppelganger(
    label,
    plot(gabor_filtered_image, axes = F)
  )

  label = paste('combined kernel spectrum')
  combined_gabor_spectrum = fftwtools::fftw2d(combined_gabor_kernel)
  vdiffr::expect_doppelganger(
    label,
    plot_matrix(combined_gabor_spectrum)
  )

  label = paste('combined reconstructed kernel')
  combined_gabor_signal <- fftwtools::fftw2d(combined_gabor_spectrum, inverse = 1)
  vdiffr::expect_doppelganger(
    label,
    plot_matrix(combined_gabor_signal, fft_shift = F, magnitude = F, log_scaling = F)
  )

})

test_that('gabor filters with combined parameters', {

  kernel_size = 31
  gamma = 0.8
  eta = 0.8
  frequency = 0.1
  phase = 0
  orientations <- c(0, 1/4, 1/2, 3/4) * pi

  combined_gabor_kernel = matrix(0 + 0i, nrow = kernel_size, ncol = kernel_size)

  for (orientation in orientations) {

    gabor_kernel <- create_gabor_kernel(kernel_size, gamma, eta, orientation, frequency, psi)
    combined_gabor_kernel =  combined_gabor_kernel + gabor_kernel
  }

  label = paste('combined kernel')
  vdiffr::expect_doppelganger(
    label,
    plot_matrix(combined_gabor_kernel, fft_shift = F, magnitude = F, log_scaling = F)
  )

  label = paste('Combined Lenna Gabor Filtered')
  image_file_path <- test_path("images", "Lenna.png")
  expected_original <- imager::load.image(image_file_path)
  expected_original_dim <- dim(expected_original)
  expected_grayscale_image = imager::grayscale(expected_original)
  expected_grayscale_image_matrix = as.matrix(expected_grayscale_image)
  # Perform convolution with real and imaginary components
  response <- imager::convolve(imager::as.cimg(expected_grayscale_image_matrix), imager::as.cimg(Re(combined_gabor_kernel))) +
    1i * imager::convolve(imager::as.cimg(expected_grayscale_image_matrix), imager::as.cimg(Im(combined_gabor_kernel)))
  # Return the magnitude of the response as an image
  gabor_filtered_image = imager::as.cimg(Mod(response), dim = dim(grayscale_matrix))
  vdiffr::expect_doppelganger(
    label,
    plot(gabor_filtered_image, axes = F)
  )

  label = paste('Combined Ma Dukes Gabor Filtered')
  image_file_path <- test_path("images", "MaDukes.png")
  expected_original <- imager::load.image(image_file_path)
  expected_original_dim <- dim(expected_original)
  expected_grayscale_image = imager::grayscale(expected_original)
  expected_grayscale_image_matrix = as.matrix(expected_grayscale_image)
  # Perform convolution with real and imaginary components
  response <- imager::convolve(imager::as.cimg(expected_grayscale_image_matrix), imager::as.cimg(Re(combined_gabor_kernel))) +
    1i * imager::convolve(imager::as.cimg(expected_grayscale_image_matrix), imager::as.cimg(Im(combined_gabor_kernel)))
  # Return the magnitude of the response as an image
  gabor_filtered_image = imager::as.cimg(Mod(response), dim = dim(grayscale_matrix))
  vdiffr::expect_doppelganger(
    label,
    plot(gabor_filtered_image, axes = F)
  )

  label = paste('combined kernel spectrum')
  combined_gabor_spectrum = fftwtools::fftw2d(combined_gabor_kernel)
  vdiffr::expect_doppelganger(
    label,
    plot_matrix(combined_gabor_spectrum)
  )

  label = paste('combined reconstructed kernel')
  combined_gabor_signal <- fftwtools::fftw2d(combined_gabor_spectrum, inverse = 1)
  vdiffr::expect_doppelganger(
    label,
    plot_matrix(combined_gabor_signal, fft_shift = F, magnitude = F, log_scaling = F)
  )

})

image_media <- function(x) {
  if (!(is.character(x) && file.exists(x))) {
    stop("Invalid input: Expected a valid file path to a media file.")
  }

  # Load and process the image
  original_image <- imager::load.image(x)
  original_dimensions <- dim(original_image)
  grayscale_image <- imager::grayscale(original_image)
  grayscale_matrix <- as.matrix(grayscale_image)

  # Compute FFT components
  idealized_spectrum <- fftwtools::fftw2d(grayscale_matrix)
  idealized_signal <- fftwtools::fftw2d(idealized_spectrum, inverse = 1)
  idealized_dimensions <- original_dimensions
  idealized_dimensions[length(idealized_dimensions)] <- 1
  idealized_image <- imager::as.cimg(Re(idealized_signal), dim = idealized_dimensions)

  # Method for Gabor-filtered images
  gabor_filtered_image <- function(orientation, f = 0.2, kernel_size = 31) {
    k     <- 2
    p     <- 0.5
    n     <- 4
    gamma <- (1/pi) * (k+1)/(k-1) * sqrt(-log(p))
    eta   <- (1/pi) * sqrt(-log(p)) / (pi / (2 * n))

    # Generate Gabor kernels
    kernel_real <- create_gabor_kernel(kernel_size, gamma, eta, orientation, f, 0)
    kernel_imag <- create_gabor_kernel(kernel_size, gamma, eta, orientation, f, pi / 2)

    # Perform convolution
    response_real <- imager::convolve(imager::as.cimg(grayscale_matrix), imager::as.cimg(kernel_real))
    response_imag <- imager::convolve(imager::as.cimg(grayscale_matrix), imager::as.cimg(kernel_imag))

    # Compute magnitude of the response
    response <- sqrt(as.matrix(response_real)^2 + as.matrix(response_imag)^2)
    return(imager::as.cimg(response, dim = dim(grayscale_matrix)))
  }

  # Create the S3 object
  obj <- list(
    original_image       = original_image,
    original_dimensions  = original_dimensions,
    idealized_spectrum   = idealized_spectrum,
    idealized_dimensions = idealized_dimensions,
    idealized_signal     = idealized_signal,
    idealized_image      = idealized_image,
    gabor_filtered_image = gabor_filtered_image
  )

  # Assign S3 class
  class(obj) <- "image_media"
  return(obj)
}

create_gabor_kernel <- function(kernel_size, gamma, eta, orientation, f, psi) {
  if (kernel_size %% 2 == 0) {
    stop("Kernel size must be odd.")
  }

  half_size <- (kernel_size - 1) / 2
  x <- seq(-half_size, half_size, length.out = kernel_size)
  y <- seq(-half_size, half_size, length.out = kernel_size)
  grid <- expand.grid(x = x, y = y)

  # Rotate coordinates
  xr <- grid$x * cos(orientation) + grid$y * sin(orientation)
  yr <- -grid$x * sin(orientation) + grid$y * cos(orientation)

  # Scaling factor for the Gaussian envelope
  scaling_factor <- f^2 / (pi * gamma * eta)

  # Generate the Gabor kernel
  gabor <- scaling_factor *
    exp(-((f^2 * xr^2) / (gamma^2) + (f^2 * yr^2) / (eta^2))) *  # No factor 2
    cos(2 * pi * f * xr + psi)

  # Reshape into a matrix
  kernel <- matrix(gabor, nrow = kernel_size, ncol = kernel_size)
  return(kernel)
}

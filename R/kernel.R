#' Convolution with Kernel
#'
#' Applies a kernel to a complex matrix
#'
#' @param complex_matrix The matrix to convolve with
#' @param kernel The kernel
#' @return A convolved matrix
convolution_with_kernel <- function(complex_matrix, kernel) {
  imager::convolve(imager::as.cimg(Re(complex_matrix)), imager::as.cimg(Re(kernel))) +
    1i * imager::convolve(imager::as.cimg(Im(complex_matrix)), imager::as.cimg(Im(kernel)))
}

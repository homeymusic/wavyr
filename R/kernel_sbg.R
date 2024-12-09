#' Create SBG Kernel
#'
#' Generates a SBG kernel with specific parameters.
#'
#' @param kernel_size Size of the kernel (must be odd).
#' @param uncertainty Uncertainty product
#' @param signal_or_spectrum Should the gabor filter use the spectrum or signal representation
#' @return A 2D matrix representing the SBG kernel.
kernel_sbg <- function(kernel_size,
                       uncertainty = GABOR_UNCERTAINTY ^ 2,
                       signal_or_spectrum = SIGNAL_OR_SPECTRUM$spectrum) {

  sparse <- sparse_spectrum_sbg(kernel_size, uncertainty)
  sparse_summarized <- sparse %>%
    dplyr::group_by(rationalized_x, rationalized_y) %>%
    dplyr::summarize(count = dplyr::n(), .groups = "drop")

  dense_spectrum_sbg <- dense_kernel_from(sparse_summarized, kernel_size)

  if (signal_or_spectrum == SIGNAL_OR_SPECTRUM$spectrum) {
    return(dense_spectrum_sbg)
  } else if (signal_or_spectrum == SIGNAL_OR_SPECTRUM$signal) {
    return(fftwtools::fftw2d(dense_spectrum_sbg, inverse = 1))
  }
}

#' Sparse SBG kernel of max size
#'
#' Generates a sparse SBG kernel with specific parameters.
#'
#' @param kernel_size Size of the kernel (must be odd).
#' @param uncertainty Uncertainty product
#' @return A 2D matrix representing the SBG kernel.
sparse_spectrum_sbg <- function(kernel_size, uncertainty) {

  if (kernel_size %% 2 == 0) {
    stop("Kernel size must be odd.")
  }

  sparse <- data_rds({
    sparse_spectrum_sbg_cpp(kernel_size, kernel_size, uncertainty)
  }, filename = paste0("spectrum_sbg_", sprintf("%.4f", uncertainty),
                       "_", kernel_size, "x", kernel_size, ".rds"))

  sparse %>% dplyr::filter(abs(rationalized_x) <= kernel_size, abs(rationalized_y) <= kernel_size)

}

dense_kernel_from <- function(sparse_df, size) {
  # Ensure the size is odd
  if (size %% 2 == 0) {
    stop("The size parameter must be an odd number.")
  }

  # Ensure the size is large enough
  max_value <- max(abs(sparse_df$rationalized_x), abs(sparse_df$rationalized_y))
  if (size < 2 * max_value + 1) {
    stop("The size parameter must be greater than or equal to the maximum range of rationalized_x and rationalized_y.")
  }

  # Initialize a dense matrix of zeros
  dense_matrix <- matrix(0, nrow = size, ncol = size)

  # Compute the center index of the matrix
  center <- (size + 1) / 2

  # Map sparse matrix coordinates to the dense matrix
  for (i in seq_len(nrow(sparse_df))) {
    x <- sparse_df$rationalized_x[i]
    y <- sparse_df$rationalized_y[i]
    count <- sparse_df$count[i]

    # Map to dense matrix indices
    row <- center - y
    col <- center + x
    dense_matrix[row, col] <- count
  }

  return(dense_matrix)
}

SIGNAL_OR_SPECTRUM <- data.frame(
  signal = 'signal',
  spectrum = 'spectrum'
)

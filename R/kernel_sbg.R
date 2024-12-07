#' Create SBG Kernel
#'
#' Generates a SBG kernel with specific parameters.
#'
#' @param kernel_size Size of the kernel (must be odd).
#' @return A 2D matrix representing the SBG kernel.
kernel_sbg <- function(kernel_size) {
  if (kernel_size %% 2 == 0) {
    stop("Kernel size must be odd.")
  }

  fourier_transform_2D_map <- function(nrows, ncols, uncertainty = GABOR_UNCERTAINTY ^ 2) {
    data_rds({
      fourier_transform_2D_map_cpp(nrows, ncols, uncertainty)
    }, filename = paste0("fourier_transform_2D_map_", sprintf("%.4f", uncertainty),
                         "_", nrows, "x", ncols, ".rds"))
  }



  return(kernel)
}

center_submatrix <- function(matrix_m, n) {
  # Ensure the input matrix is square and odd-dimensioned
  m <- nrow(matrix_m)
  if (m != ncol(matrix_m)) {
    stop("The input matrix must be square.")
  }
  if (m %% 2 == 0) {
    stop("The input matrix must have an odd dimension.")
  }
  if (n %% 2 == 0) {
    stop("The size of the submatrix must be odd.")
  }
  if (n > m) {
    stop("The submatrix size n must be less than or equal to the size of the input matrix m.")
  }

  # Calculate the center of the matrix
  center <- (m + 1) / 2

  # Calculate the indices for the submatrix
  half_n <- (n - 1) / 2
  row_start <- center - half_n
  row_end <- center + half_n
  col_start <- center - half_n
  col_end <- center + half_n

  # Extract the submatrix
  return(matrix_m[row_start:row_end, col_start:col_end])
}

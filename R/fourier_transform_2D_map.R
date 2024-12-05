#' @export
fourier_transform_2D_map <- function(nrows, ncols, uncertainty = GABOR_UNCERTAINTY ^ 2) {
  data_rds({
    fourier_transform_2D_map_cpp(nrows, ncols, uncertainty)
  }, filename = paste0("fourier_transform_2D_map_", sprintf("%.4f", uncertainty),
                       "_", nrows, "x", ncols, ".rds"))
}

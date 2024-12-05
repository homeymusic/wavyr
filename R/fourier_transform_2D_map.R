#' @export
fourier_transform_2D_map <- function(nrows, ncols) {
  data_rds({
    fourier_transform_2D_map_cpp(nrows, ncols, GABOR_UNCERTAINTY ^ 2)
  }, filename = paste0("fourier_transform_2D_map_", nrows, "x", ncols, ".rds"))
}

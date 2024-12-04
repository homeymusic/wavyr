#' @export
rationalized_spatial_frequency_map <- function(nrows, ncols) {
  data_rds({
    rationalized_spatial_frequency_map_cpp(nrows, ncols, GABOR_UNCERTAINTY ^ 2)
  }, filename = paste0("rationalized_spatial_frequency_map_", nrows, "x", ncols, ".rds"))
}

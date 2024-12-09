#' @export
spectrum_sbg <- function(nrows, ncols, uncertainty = GABOR_UNCERTAINTY ^ 2) {
  data_rds({
    spectrum_sbg_cpp(nrows, ncols, uncertainty)
  }, filename = paste0("spectrum_sbg_", sprintf("%.4f", uncertainty),
                       "_", nrows, "x", ncols, ".rds"))
}

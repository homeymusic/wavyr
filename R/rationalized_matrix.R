#' @export
rationalized_matrix <- function(rows, columns) {
  structure(
    list(
      rows = rows,
      columns = columns,
      matrix = data_rds({
        rationalized_spatial_frequency_map_cpp(
          idealized_matrix(rows, columns)$matrix,
          GABOR_UNCERTAINTY ^ 2
        )
      }, filename = paste0("rationalized_matrix_", rows, "x", columns, ".rds"))
    ),
    class = "rationalized_matrix"
  )
}

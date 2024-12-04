#' @export
idealized_matrix <- function(rows, columns) {
  structure(
    list(
      rows = rows,
      columns = columns,
      matrix = data_rds({
        idealized_spatial_frequency_map_cpp(rows, columns)
      }, filename = paste0("idealized_matrix_", rows, "x", columns, ".rds"))
    ),
    class = "idealized_matrix"
  )
}

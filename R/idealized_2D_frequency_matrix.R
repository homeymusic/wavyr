#' @export
idealized_2D_frequency_matrix <- function(rows, columns) {
  structure(
    list(
      rows = rows,
      columns = columns,
      matrix = idealized_spatial_frequency_map_cpp(rows, columns)
    ),
    class = "idealized_2D_frequency_map"
  )
}

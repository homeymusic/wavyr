#' @export
linear_wavenumber <- function(x) {

  metadata = LINEAR_WAVENUMBER

  property_obj <- property(x, metadata)

  structure(
    property_obj,
    class = c(LINEAR_WAVENUMBER$class_name, class(property_obj))
  )

}


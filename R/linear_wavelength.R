#' @export
linear_wavelength <- function(x) {

  metadata = LINEAR_WAVELENGTH

  property_obj <- property(x, metadata)

  structure(
    property_obj,
    class = c(LINEAR_WAVELENGTH$class_name, class(property_obj))
  )

}


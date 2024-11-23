#' @export
angular_wavelength <- function(x) {

  metadata = ANGULAR_WAVELENGTH

  property_obj <- property(x, metadata)

  structure(
    property_obj,
    class = c(ANGULAR_WAVELENGTH$class_name, class(property_obj))
  )

}

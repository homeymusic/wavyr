#' @export
angular_wavenumber <- function(x) {

  metadata = ANGULAR_WAVENUMBER

  property_obj <- property(x, metadata)

  structure(
    property_obj,
    class = c(ANGULAR_WAVENUMBER$class_name, class(property_obj))
  )

}


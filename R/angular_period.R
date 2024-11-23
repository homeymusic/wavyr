#' @export
angular_period <- function(x) {

  metadata = ANGULAR_PERIOD

  property_obj <- property(x, metadata)

  structure(
    property_obj,
    class = c(ANGULAR_PERIOD$class_name, class(property_obj))
  )

}

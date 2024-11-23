#' @export
angular_frequency <- function(x) {

  metadata = ANGULAR_FREQUENCY

  property_obj <- property(x, metadata)

  structure(
    property_obj,
    class = c(ANGULAR_FREQUENCY$class_name, class(property_obj))
  )

}

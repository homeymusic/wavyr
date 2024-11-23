#' @export
linear_period <- function(x) {

  metadata = LINEAR_PERIOD

  property_obj <- property(x, metadata)

  structure(
    property_obj,
    class = c(LINEAR_PERIOD$class_name, class(property_obj))
  )

}


#' @export
linear_frequency <- function(x) {

  metadata = LINEAR_FREQUENCY

  property_obj <- property(x, metadata)

  structure(
    property_obj,
    class = c(LINEAR_FREQUENCY$class_name, class(property_obj))
  )

}


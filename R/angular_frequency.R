#' @export
angular_frequency <- function(x) {

  class_name   = "angular_frequency"

  metadata = list(
    name         = "angular frequency",
    unit         = "rad/s",
    unit_latex   = "\\frac{\\text{rad}}{\\text{s}}",
    symbol       = "\u03C9",
    symbol_latex = "\\omega",
    linear_angular     = LINEAR_ANGULAR$angular,
    space_time    = SPACE_TIME$time,
    rate_extent      = RATE_EXTENT$rate,
    class_name   = class_name
  )

  property_obj <- property(x, metadata)

  structure(
    property_obj,
    class = c(class_name, class(property_obj))
  )

}

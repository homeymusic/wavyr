#' @export
angular_period <- function(x) {

  class_name   = "angular_period"

  metadata = list(
    name         = "angular period",
    unit         = "s/rad",
    unit_latex   = "\\frac{\\text{s}}{\\text{rad}}",
    symbol       = "T_angular",
    symbol_latex = "T_\\text{angular}",
    linear_angular     = LINEAR_ANGULAR$angular,
    space_time    = SPACE_TIME$time,
    rate_extent      = RATE_EXTENT$extent,
    class_name   = class_name
  )

  property_obj <- property(x, metadata)

  structure(
    property_obj,
    class = c(class_name, class(property_obj))
  )

}

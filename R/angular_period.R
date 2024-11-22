#' @export
angular_period <- function(x) {

  class_name   = "angular_period"

  metadata = list(
    name         = "angular period",
    unit         = "s/rad",
    unit_latex   = "\\frac{\\text{s}}{\\text{rad}}",
    symbol       = "T_angular",
    symbol_latex = "T_\\text{angular}",
    rotation     = Rotation$angular,
    dimension    = Dimension$temporal,
    measure      = Measure$extent,
    class_name   = class_name
  )

  property_obj <- property(x, metadata)

  structure(
    property_obj,
    class = c(class_name, class(property_obj))
  )

}

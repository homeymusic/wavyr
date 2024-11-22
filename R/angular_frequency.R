#' @export
angular_frequency <- function(x) {

  class_name   = "angular_frequency"

  metadata = list(
    name         = "angular frequency",
    unit         = "rad/s",
    unit_latex   = "\\frac{\\text{rad}}{\\text{s}}",
    symbol       = "\u03C9",
    symbol_latex = "\\omega",
    rotation     = Rotation$angular,
    dimension    = Dimension$temporal,
    measure      = Measure$rate,
    class_name   = class_name
  )

  property_obj <- property(x, metadata)

  structure(
    property_obj,
    class = c(class_name, class(property_obj))
  )

}


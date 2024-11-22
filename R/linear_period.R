#' @export
linear_period <- function(x) {

  class_name   = "linear_period"

  metadata = list(
    name         = "linear period",
    symbol_latex = "T",
    symbol       = "T",
    unit_latex   = "\\text{s}",
    unit         = "s",
    rotation     = Rotation$linear,
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


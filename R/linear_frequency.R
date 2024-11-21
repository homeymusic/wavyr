#' @export
linear_frequency <- function(x) {

  class_name   = "linear_frequency"

  metadata = list(
    name         = "linear frequency",
    symbol_latex = "f",
    symbol       = "f",
    unit_latex   = "Hz",
    unit         = "Hz",
    rotation     = Rotation$linear,
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

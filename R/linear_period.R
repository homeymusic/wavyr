#' @export
linear_period <- function(x) {

  class_name   = "linear_period"

  metadata = list(
    name         = "linear period",
    symbol_latex = "T",
    symbol       = "T",
    unit_latex   = "\\text{s}",
    unit         = "s",
    rotation     = LINEAR_ANGULAR$linear,
    dimension    = SPACE_TIME$temporal,
    measure      = RATE_EXTENT$extent,
    class_name   = class_name
  )

  property_obj <- property(x, metadata)

  structure(
    property_obj,
    class = c(class_name, class(property_obj))
  )

}


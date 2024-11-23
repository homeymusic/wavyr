#' @export
linear_frequency <- function(x) {

  class_name   = "linear_frequency"

  metadata = list(
    name         = "linear frequency",
    symbol_latex = "f",
    symbol       = "f",
    unit_latex   = "\\text{Hz}",
    unit         = "Hz",
    linear_angular     = LINEAR_ANGULAR$linear,
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


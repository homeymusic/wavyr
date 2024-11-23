#' @export
angular_wavenumber <- function(x) {

  class_name   = "angular_wavenumber"

  metadata = list(
    name         = "angular wavenumber",
    unit         = "rad/m",
    unit_latex   = "\\frac{\\text{rad}}{\\text{m}}",
    symbol       = "k_angular",
    symbol_latex = "k_\\text{angular}",
    rotation     = LINEAR_ANGULAR$angular,
    dimension    = SPACE_TIME$spatial,
    measure      = RATE_EXTENT$rate,
    class_name   = class_name
  )

  property_obj <- property(x, metadata)

  structure(
    property_obj,
    class = c(class_name, class(property_obj))
  )

}


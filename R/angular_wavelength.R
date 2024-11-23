#' @export
angular_wavelength <- function(x) {

  class_name   = "angular_wavelength"

  metadata = list(
    name         = "angular wavelength",
    unit         = "m/rad",
    unit_latex   = "\\frac{\\text{m}}{\\text{rad}}",
    symbol       = "l_angular",
    symbol_latex = "\\lambda_\\text{angular}",
    linear_angular     = LINEAR_ANGULAR$angular,
    space_time    = SPACE_TIME$space,
    rate_extent      = RATE_EXTENT$extent,
    class_name   = class_name
  )

  property_obj <- property(x, metadata)

  structure(
    property_obj,
    class = c(class_name, class(property_obj))
  )

}

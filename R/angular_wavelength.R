#' @export
angular_wavelength <- function(x) {

  class_name   = "angular_wavelength"

  metadata = list(
    name         = "angular wavelength",
    unit         = "m/rad",
    unit_latex   = "\\frac{\\text{m}}{\\text{rad}}",
    symbol       = "l_angular",
    symbol_latex = "\\lambda_\\text{angular}",
    rotation     = Rotation$angular,
    dimension    = Dimension$spatial,
    measure      = Measure$extent,
    class_name   = class_name
  )

  property_obj <- property(x, metadata)

  structure(
    property_obj,
    class = c(class_name, class(property_obj))
  )

}

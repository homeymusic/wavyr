#' @export
linear_wavelength <- function(x) {

  class_name   = "linear_wavelength"

  metadata = list(
    name         = "linear wavelength",
    symbol_latex = "\\lambda",
    symbol       = "\u03BB",
    unit_latex   = "\\text{m}",
    unit         = "m",
    rotation     = Rotation$linear,
    dimension    = Dimension$spatial,
    measure      = Measure$extent,
    class_name   = class_name
  )

  property_obj <- property(x, metadata)

  structure(
    property_obj,
    class = c("linear_wavelength", class(property_obj))
  )

}

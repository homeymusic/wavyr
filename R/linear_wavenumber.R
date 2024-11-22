#' @export
linear_wavenumber <- function(x) {

  class_name     = "linear_wavenumber"

  metadata = list(
    name         = "linear wavenumber",
    symbol_latex = "k_{\\text{linear}}",
    symbol       = "k_linear",
    unit_latex   = "\\text{m}^{-1}",
    unit         = "1/m",
    rotation     = Rotation$linear,
    dimension    = Dimension$spatial,
    measure      = Measure$rate,
    class_name   = class_name
  )

  property_obj <- property(x, metadata)

  structure(
    property_obj,
    class = c("linear_wavenumber", class(property_obj))
  )

}


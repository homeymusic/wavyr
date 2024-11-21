#' @export
angular_frequency <- function(value) {
  # Create the base property object
  property_obj <- property(value)

  # Override specific attributes and add additional classes
  structure(
    modifyList(property_obj, list(
      unit = "rad/s",       # Unit for angular frequency
      unit_latex = "\\frac{\\mathrm{rad}}{\\mathrm{s}}", # LaTeX representation for rad/s
      symbol = "\u03C9",    # Unicode for omega (ω)
      symbol_latex = "\\omega", # LaTeX representation for omega
      name = "angular frequency" # Descriptive name
    )),
    class = c("angular_frequency", "angular", "temporal", "rate", "property")
  )
}

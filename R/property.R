# Define the generic property constructor
#' @export
property <- function(x) {
  UseMethod("property")
}

# Internal function for creating a property object
.property <- function(x) {
  structure(
    list(
      value = x,
      unit = "unit",
      unit_latex = "latex unit",
      symbol = "symbol",
      symbol_latex = "latex symbol",
      name = "name"
    ),
    class = "property"
  )
}

# Method for creating a property from a numeric value
#' @export
property.numeric <- function(x) {
  if (length(x) != 1) {
    stop("`value` must be of length 1")
  }
  .property(x)
}

# Method for creating a property from another property object
#' @export
property.property <- function(x) {
  if (length(x$value) != 1) {
    stop("`value` must be of length 1")
  }
  .property(convert(x)$value)
}

#' @export
property.default <- function(value) {
  stop("`value` must be numeric or a property object")
}

# Define the generic property constructor
#' @export
convert <- function(x) {
  UseMethod("convert")
}

#' @export
convert.property <- function(x) {
  x
}

#' @export
as_angular_frequency <- function(x, ...) {
  UseMethod("as_angular_frequency")
}

#' @export
as_linear_wavelength <- function(x, ...) {
  UseMethod("as_linear_wavelength")
}


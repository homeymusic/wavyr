# Define the generic property constructor
#' @export
property <- function(x, metadata) {
  UseMethod("property")
}

# Internal function for creating a property object
.property <- function(x, metadata = list()) {

  structure(
    modifyList(metadata, list(
      value = x
    )),
    class = 'property'
  )

}

# Method for creating a property from a numeric value
#' @export
property.numeric <- function(x, metadata=list()) {
  if (length(x) != 1) {
    stop("`x` must be of length 1")
  }
  .property(x, metadata)
}

# Method for creating a property from another property object
#' @export
property.property <- function(x, metadata=list()) {
  if (length(x$value) != 1) {
    stop("`x` must be of length 1")
  }

  if (length(metadata) == 0 || class(x)[1] == metadata$class_name) {
    return(x)
  }

  value = x$value

  if (x$dimension != metadata$dimension) {
    value = value / DEFAULT_SPEED_OF_MEDIUM
  }

  if (x$measure != metadata$measure) {
    value = 1 / value
  }

  if (x$rotation != metadata$rotation) {
    value = 2 * pi * value
  }

  .property(value, metadata = metadata)
}

#' @export
property.default <- function(x) {
  stop("`x` must be numeric or a property object")
}

#' @export
Dimension <- list(spatial = "spatial", temporal = "temporal")

#' @export
Rotation  <- list(linear = "linear", angular = "angular")

#' @export
Measure   <- list(extent = "extent", rate = "rate")

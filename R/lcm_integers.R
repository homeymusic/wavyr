#' Least Common Multiple of Integer Vector
#'
#' Computes the least common multiple of a vector of integers.
#'
#' @param x A numeric vector of integers.
#' @return A single numeric value representing the least common multiple.
#' @export
lcm_integers <- function(x) {
  Reduce(gmp::lcm.bigz, x) %>% as.numeric()
}

#' A quadratic function
#'
#' @param x a numeric value
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' myquad(x = 1:10)
myquad <- function(x) {
  x^2 - 5 * x + 6 + 2
}

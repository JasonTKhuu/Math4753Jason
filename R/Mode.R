#' Mode
#'
#' @param v gets the vector
#'
#' @return the most occurrence of a numeric value
#' @export
#'
#' @examples
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

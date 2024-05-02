#' Curve
#'
#' @param mu gets given mean
#' @param sigma gets given sigma
#' @param a gets a (which is x)
#'
#' @return a plot of the given values
#' @export
#'
#' @examples
myncurve = function(mu, sigma, a){
  x <- NULL
  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3 * sigma, mu + 3 * sigma))

  xcurve <- seq(1,5, length = 1000)
  ycurve <- dnorm(xcurve, mean = mu, sd = sigma)

  polygon(c(1, xcurve, 5), c(0, ycurve, 0), col = "pink")

  prob <- pnorm(5, mean = mu, sd = sigma) - pnorm(1, mean = mu, sd = sigma)
  prob <- round(prob, 4)

  text(prob, dnorm(4, mu, sigma), paste("Area = ", prob, sep = ""))

  list(mu = mu, sigma = sigma, a = a, area = prob)
}


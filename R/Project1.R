#' Discrete and Continuous ticket probability
#'
#' @param N gets the number of seats in flight
#' @param gamma gets the probability that the plane will be overbooked
#' @param p gets the probability that the person will actually show up
#'
#' @return two charts with a continuous and discrete plotting of where the optimal tickets should be sold
#' @importFrom grDevices rainbow
#' @importFrom graphics abline barplot curve hist layout polygon segments text title
#' @importFrom stats dnorm dpois pbinom pnorm qbinom qnorm quantile rpois uniroot
#' @export
#'
#' @examples
ntickets <- function(N, gamma, p) {
  nd <- qbinom(1 - gamma, size = N, prob = p)
  nd <- (2 * N) - (nd)

  mu <- N * p
  sigma <- sqrt(N * p * (1 - p))
  nc <- qnorm(1 - gamma, mean = mu, sd = sigma)
  nc <- (2 * N)-(nc)

  result <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)

  discrete <- function(n) {
    (1 - gamma) - pbinom(N, round(n), p)
  }

  continuous <- function(n) {
    (1 - gamma) - pnorm(N + 0.5, n * p, sqrt(n * p * (1 - p)))
  }

  curve(expr = discrete, from = N, to = N + 20, col = "black", xlab="n",ylab="Objective")
  unirootDis <- uniroot(f = discrete, interval = c(N - 5, N + 25))
  abline(v = unirootDis, h = 0, lty = "solid", col = "red")

  title(main=paste("Objective Vs n to find optimal tickets sold (Discrete -",nd,")"))

  curve(expr = continuous, from = N, to = N + 20, col = "black", xlab="n",ylab="Objective")
  unirootCont <- uniroot(f = continuous, interval = c(N - 5, N + 25))
  abline(v = unirootCont, h = 0, lty = "solid", col = "blue")

  title(main=paste("Objective Vs n to find optimal tickets sold (Continuous -",floor(nc),")"))
  print(result)
}

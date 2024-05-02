#' Poisson Likelihood
#'
#' @param theta1 Gets the first theta given
#' @param theta2 Gets the second theta given
#' @param lfun Gets the function that will be used (going to have to create your own)
#' @param ... Anything else can be added so things like title and moving the text etc.
#'
#' @return A plot of what the Poisson likelihood will look like given the theta's and function
#' @export
#'
#' @examples
maxlikg2=function(theta1,theta2,lfun="logbinpois",...){
  n1=length(theta1)
  n2=length(theta2)
  z=outer(theta1,theta2,lfun)
  contour(theta1,theta2,exp(z),...)
  maxl=max(exp(z))
  coord=which(exp(z)==maxl,arr.ind=TRUE)
  th1est=theta1[coord[1]]
  th2est=theta2[coord[2]]
  abline(v=th1est,h=th2est)
  axis(3,th1est,round(th1est,2))
  axis(4,th2est,round(th2est,2),las=1)
  list(th1est=th1est,th2est=th2est)
}

#' The Poisson Sample
#'
#' @param n gets the sample size
#' @param iter gets the amount of times the function will iterate
#' @param lambda represents the Poisson distribution which is defaulted to 10
#' @param ... any values can be added onto here so maybe things like graphical design.
#'
#' @return a graph showing the n, iteration, and poisson distribution
#' @export
#'
#' @examples
mycltp=function(n,iter,lambda=10,...){

  y=rpois(n*iter,lambda=lambda)

  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)

  w=apply(data,2,mean)

  param=hist(w,plot=FALSE)

  ymax=max(param$density)
  ymax=1.1*ymax

  layout(matrix(c(1,1,2,3),nrow=2,ncol=2, byrow=TRUE))

  hist(w,freq=FALSE,  ylim=c(0,ymax), col=rainbow(max(w)),
       main=paste("Histogram of sample mean","\n", "sample size= ",n," iter=",iter," lambda=",lambda,sep=""),
       xlab="Sample mean",...)

  curve(dnorm(x,mean=lambda,sd=sqrt(lambda/n)),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve

  barplot(table(y)/(n*iter),col=rainbow(max(y)), main="Barplot of sampled y", ylab ="Rel. Freq",xlab="y" )
  x=0:max(y)
  plot(x,dpois(x,lambda=lambda),type="h",lwd=5,col=rainbow(max(y)),
       main="Probability function for Poisson", ylab="Probability",xlab="y")
}

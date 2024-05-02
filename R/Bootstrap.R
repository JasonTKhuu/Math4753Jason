
#' BootStrap
#'
#' @param iter iterates a number of times given (or 10000 by default)
#' @param x gets the sample (random)
#' @param fun determines which function to use with mean being default
#' @param alpha determines the confidence interval with 0.05 being default (95% confidence)
#' @param cx determines position in terms of x (1.5 by default)
#' @param ... anything else can be added to this such as xlab and more
#'
#' @return a histogram of the bootstrap samples given
#' @export
#'
#' @examples
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){
  x <- NULL
  n=length(x)

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun)
  ci=quantile(xstat,c(alpha/2,1-alpha/2))
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)

  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")
  segments(ci[1],0,ci[2],0,lwd=4)
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x))
}

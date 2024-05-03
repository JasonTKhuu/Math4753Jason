#' Bootstrap
#'
#' @param iter this value is basically the amount of times the sample will loop
#' @param x a vector of the data set
#' @param fun the function used for the bootstrap (example mean, median, etc)
#' @param alpha the alpha value is used to help determine quantile
#' @param cx the cx value is basically the controlled x value
#' @param ... this can accept any values with things such as xlab
#'
#' @return this will return a bootstrap histogram of the values given through the function
#' @export
#'
#' @examples
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){
  n=length(x)

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun)
  ci=quantile(xstat,c(alpha/2,1-alpha/2))
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)

  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")
  segments(ci[1],0,ci[2],0,lwd=4)
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x))
}

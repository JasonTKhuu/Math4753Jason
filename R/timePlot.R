#' timePlot
#'
#' @param n a numeric value (default value of 1000)
#' @param iter a numeric value that iterates (default value of 10)
#' @param time a numeric value that determines the time (default value of 0.5)
#'
#' @return a animation of the samples being plotted based on the iter, n, and time value.
#' @export
#'
#' @examples
mysample=function(n = 1000, iter=10,time=0.5){
  for( i in 1:iter){
    s=sample(1:10,n,replace=TRUE)
    sf=factor(s,levels=1:10)
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )

    Sys.sleep(time)
  }
}

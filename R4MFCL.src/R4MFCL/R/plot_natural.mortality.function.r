#'
#' @export
plot_natural.mortality.function <- function(repfiles=list(read.rep(baserep),
read.rep(baserep)),modlab = c("Base","Base"))
{
# Plot M-at-age from some different runs
# Plots basecase last

    M <- list()
    nSps<-vector(mode="numeric",length=length(repfiles))
    for(i in 1:length(repfiles))
    {
        M[[i]] <- repfiles[[i]]$MatAge
        nSps[i]<-ifelse(is.null(repfiles[[i]]$nSp),-1,repfiles[[i]]$nSp)
    }


    # get the maximum value across the runs
    maxy <- max(unlist(lapply(M,max)))
#    browser()
    # if only one series then just make a plot
    if(length(repfiles)==1)
    {
      if(nSps[1]==1){
        plot(1:length(M[[1]]), M[[1]], type="n", ylab="Natural mortality", xlab="Age class", ylim=c(0,maxy), las=1)
        lines(1:length(M[[1]]), M[[1]], lwd=2, col="black")
      }else{
        matplot(1:dim(M[[1]])[2], t(M[[1]]), type="n", ylab="Natural mortality", xlab="Age class", ylim=c(0,maxy), las=1)
        matlines(x=1:dim(M[[1]])[2], t(M[[1]]), lwd=2, col=1:dim(M[[1]])[1])
        if( nSps[1]==2 & sum(repfiles[[1]]$spSexPtr)==1){
          modlab[which(repfiles[[1]]$spSexPtr==1)]<-"Female"
          modlab[which(repfiles[[1]]$spSexPtr==0)]<-"Male"
        }
        legend("topright", lwd=2, col=1:dim(M[[1]])[1], lty=c(1,1), legend=modlab,bty="n")
      }
    }else{
      plot(1:length(M[[1]]), M[[1]], type="n", ylab="Natural mortality", xlab="Age class", ylim=c(0,maxy), las=1)

          for(i in 2:length(repfiles))
          {
              lines(1:length(M[[i]]), M[[i]], lwd=2, col=i)
          }

          lines(1:length(M[[1]]), M[[1]], lwd=2, col="black")
          legend("topright", lwd=2, col=1:length(M), lty=c(1,1), legend=modlab,bty="n")
    }
}

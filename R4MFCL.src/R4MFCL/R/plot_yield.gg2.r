#' Making plot of yield curve
#' @param repfile outputs of read.rep
#' @param xlimits ranges of F-multiplier
#' @param plot logical if print plot
#  ' @importFrom graphics title
#' @importFrom ggplot2 ggplot geom_line aes_string scale_color_viridis_d
#' @impoerFrom magrritr '%>%'
#' @impoerFrom dplyr filter
#' @export
plot_yield.gg2 <- function(repfiles, xlimits = c(0,5),plot=TRUE,modelnames){
	
	dat<-lapply(1:length(repfiles),function(i){
  	a <- repfiles[[i]]$Effmult
  	y <- repfiles[[i]]$Eq.yield*repfiles[[i]]$nRecs.yr
  	dat<-data.frame(a=a,y=y,model=modelnames[i])
  })
  dat<-do.call("rbind",dat)
#  opar <- par(mfcol=c(1,1), mar=c(4,4,1,2)+.1, lwd=.5,xpd=F)
  #opar <- par(mfcol=c(1,1),  lwd=.5,xpd=F)
  #on.exit(par(opar))
  pl<-ggplot(data=dat,aes(x=a,y=y/1000,color=model))+geom_line()+xlim(xlimits)
  cat("L17 ; \n") #;browser()
  #plot(x=a, y/1000, ylim=range(0,y/1000), xlim=xlimits, type='n', ann=F, axes=F)
  #lines(x=a, y/1000, lwd=2)
  #box()
#  axis(2, lwd=.1, cex.axis=1, las=1)
#  axis(1, lwd=.1, cex.axis=1)
  #title(ylab="Yield (1,000's mt per year)", cex.lab=1, xlab="Fishing mortality multiplier")
  pl<-pl+labs(y="Yield (1,000's mt per year)",  x="Fishing mortality multiplier")
  pl<-pl+scale_color_viridis_d(guide=guide_legend(title=""))
# add some lines for reference
#  lines(x=c(1,1), y=c(-100000, y[which(a==1)])/1000, lty=4, lwd=2, col="firebrick3")
# lines(x=c(-100000,1), y=c(y[which(a==1)]/1000, y[which(a==1)]/1000), lty=4, lwd=2, col="firebrick3")
#	for(i in 1:length(repfiles)){
#		y<-dat$y[which(dat$model=modelnames[i]) & which(dat$a==1)]/1000
#  	pl<-pl+geom_path(data=data.frame(x=c(0,1,1),y=c(y[which(a==1)]/1000, y[which(a==1)]/1000, 0)), aes_string(x="x",y="y"),linetype="dashed",colour="firebrick3")
#		pl<-pl+geom_path(data=data.frame(x=c(0,1,1),y=c(y, y, 0)), aes_string(x="x",y="y"),linetype="dashed",colour="firebrick3")
#  }
	pl<-pl+geom_vline(xintercept=1,color="red", linetype="dashed", size=1)
	#pl<-pl+geom_path(data=dat %>% fileter())
  if(plot)plot(pl)
  return(invisible(pl))
}





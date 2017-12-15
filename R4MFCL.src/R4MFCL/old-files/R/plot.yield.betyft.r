plot.yield.betyft <- function(repfile=read.rep(baserep)){
##-----------------------------------------------------------------------
##-----------------------------------------------------------------------
  a <- repfile$Effmult
  y <- repfile$Eq.yield*repfile$nRecs.yr
#  b <- repfile$Eq.B
#  sb <- repfile$Eq.SB

  opar <- par(mfcol=c(1,1),mar=c(4,4,1,2)+.1,lwd=.5,xpd=F)
  on.exit(par(opar))

  plot(a,y/1000,ylim=range(0,y/1000),type='n',ann=F,axes=F)
  lines(a,y/1000,lwd=2)
  box(bty='l',lwd=.1)
  axis(2,lwd=.1,cex.axis=1,las=1)
  axis(1,lwd=.1,cex.axis=1)
  title(ylab="Yield (1000 mt per year)",cex.lab=1,xlab="Fishing mortality multiplier")
# add some lines for reference
  lines(x=c(1,1),y=c(0,y[which(a==1)])/1000,lty=4,lwd=0.5,col="firebrick3")
  lines(x=c(0,1),y=c(y[which(a==1)]/1000,y[which(a==1)]/1000),lty=4,lwd=0.5,col="firebrick3")
  lines(x=c(a[which(y==max(y))],a[which(y==max(y))]),y=c(0,max(y)/1000),lty=4,lwd=0.5,col="dodgerblue2")
  lines(x=c(0,a[which(y==max(y))]),y=c(max(y)/1000,max(y)/1000),lty=4,lwd=0.5,col="dodgerblue2")
  


#  plot(a,b,ylim=range(0,b/1000),type='n',ann=F,axes=F)
#  lines(a,b/1000,lwd=2)
#  lines(a,sb/1000,lwd=2,col=2)
#  text(0,50,"Spawning potential", adj=c(0,0),col=2)
#  text(0.3,600, "Total biomass", adj=c(0,0))
#  box(bty='l',lwd=.1)
#  axis(2,lwd=.1,cex.axis=1,las=1)
#  axis(1,lwd=.1,cex.axis=1)
#  title(ylab="Equilibrium biomass (1000 mt)",cex.lab=1,
 #       )
}





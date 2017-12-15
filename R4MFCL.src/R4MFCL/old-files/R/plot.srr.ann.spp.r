plot.srr.ann.spp <- function(repfile=read.rep(baserep),legopt=list(locy=50,locx1=400,xincr=4,yincr=2)){
##-----------------------------------------------------------------------
## Stock Recruitmen relationship
##-----------------------------------------------------------------------
#varfile="yft.glm.var"

  #junk <- getBHSR(plotrepfile)

#  srr <- getrecbio.sd(varfile)
  #plot(srr$biomass/1000,srr$ucb/1000000,type='n',ylim=range(0,srr$ucb/1000000,junk$recrut/1000000),ann=F,axes=F)
  #polygon(c(srr$biomass/1000,rev(srr$biomass/1000)),c(srr$ucb/1000000,rev(srr$lcb/1000000)),border=0,
  #        col="LightSteelBlue")
  #lines(srr$biomass/1000,srr$rec/1000000,lwd=3)
  year <- seq(from=repfile$Year1,length=repfile$nTimes-1,by=1/repfile$nRecs.yr)
  year2 <- unique(trunc(year))
 #browser()
  SB <- aggregate(repfile$Obs.SB/1000,by=list(trunc(year)),mean)
  REC <- aggregate(repfile$Obs.R/1000000,by=list(trunc(year)),sum)


  plot(repfile$Pred.SB/1000,repfile$Pred.R*4/1000000,type='n',ylim=range(0,REC[,2]),ann=F,axes=F)
  lines(repfile$Pred.SB/1000,repfile$Pred.R*4/1000000,lwd=3,col=1)
  a <- length(unique(trunc(year)))
  cols <- hsv(0.75, 1:a/a, 0.8, 1)
  points(SB[,2],REC[,2],pch=16, col=cols)
#browser()
  box()
  axis(2,lwd=.1,cex.axis=1,las=1)
  axis(1,lwd=.1,cex.axis=1)
  title(ylab="Recruitment (millions)",xlab="Spawning potential",cex.lab=1)
#srr
##labels at 10 years

      for( i in 1:a){
      x1 <- legopt$locx1+i*legopt$xincr
      x2 <- legopt$locx1+(i+1)*legopt$xincr
      y1 <- legopt$locy-legopt$yincr
      y2 <- legopt$locy
      polygon(c(x1, x2, x2, x1), c(y1, y1, y2, y2), col=cols[i], border=NA)

      #label at 10 year intervals
      b <- (i)/10 - trunc((i)/10)
            if(b == 0){
            text(x1, y2 + 0.5*legopt$yincr, trunc(year2[i]), cex=0.65)
            lines(c(x1,x1), c(y1, y2+0.1*legopt$yincr), lty=1)
            }
      }


}

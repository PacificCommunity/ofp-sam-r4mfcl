plot.srr.spp <- function(repfile=read.rep(baserep),legopt=list(locy=22,locx1=150,xincr=1,yincr=0.5)){
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
  year <- repfile$Year1+seq(length(repfile$Obs.SB))/4 - .125


  plot(repfile$Pred.SB/1000,repfile$Pred.R/1000000,type='n',ylim=range(0,repfile$Obs.R/1000000),ann=F,axes=F)
  lines(repfile$Pred.SB/1000,repfile$Pred.R/1000000,lwd=3,col=1)
  a <- length(year)
  cols <- hsv(0.75, 1:a/a, 0.8, 1)
  points(repfile$Obs.SB/1000,repfile$Obs.R/1000000,pch=16, col=cols)
#browser()
  box()
  axis(2,lwd=.1,cex.axis=1,las=1)
  axis(1,lwd=.1,cex.axis=1)
  title(ylab="Recruitment (millions)",xlab="Spawning potential",cex.lab=1)
#srr
#junk
#a
##add legend
##labels at 10 years

      for( i in 1:a){
      x1 <- legopt$locx1+i*legopt$xincr
      x2 <- legopt$locx1+(i+1)*legopt$xincr
      y1 <- legopt$locy-legopt$yincr
      y2 <- legopt$locy
      polygon(c(x1, x2, x2, x1), c(y1, y1, y2, y2), col=cols[i], border=NA)

      #label at 10 year intervals
      b <- (i)/40 - trunc((i)/40)
            if(b == 0){
            text(x1, y2 + 0.5*legopt$yincr, trunc(year[i]), cex=0.65)
            lines(c(x1,x1), c(y1, y2+0.1*legopt$yincr), lty=1)
            }
      }
      

}


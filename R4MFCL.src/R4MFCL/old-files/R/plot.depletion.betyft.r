plot.depletion.betyft <- function(figdir=figdir,plotrep=read.rep(baserep),
refpoint=get.outcomes.2014(read.rep(baserep),read.par(basepar),catch.rep=basecat,nofish=T,nofishp=c(44,4))$SBlatest.SBF0,
refyear= 2009.5,
type="SSB",plotdim=c(5,2),pnames=c("Fig-deplregion","Fig-deplwcpo"),ptype="png")
{
##-----------------------------------------------------------------------
# Tidied up a little SJH 16/7/2011
##-----------------------------------------------------------------------
#### SJH 7/3/2014 7:26:34 PM make a standalone plot with the stock status on it

# Dimensioning
#time steps
nyr <- plotrep$nTimes
#first year
year1 <- plotrep$Year1
#number of time steps per year
tsteps <- plotrep$nRecs.yr
#number regions
nreg <- plotrep$nReg

year <- trunc(seq(year1,length=nyr,by=1/tsteps))


# reading in the numbers
if(type=="SSB")
{
  B <- plotrep$AdultBiomass
  Bnof <- plotrep$AdultBiomass.nofish
  textlab <- "Proportion of total spawning potential - SB/SB(F=0)"
}
else
{
  B <- plotrep$TotBiomass
  Bnof <- plotrep$TotalBiomass.nofish
  textlab <- "Proportion of total biomass"
}
  ##-- add on totals  and divide by 1000
  B <- cbind(B,apply(B,1,sum)) /1000
  Bnof <- cbind(Bnof,apply(Bnof,1,sum))  /1000

  ##--- aggregate by year
  B <- aggregate(B,list(year),mean)
  #year <- B[,1]
  #B <- B[,-1]
  Bnof <- aggregate(Bnof,list(year),mean)


  nplt <- nreg+1
  opar <- par(mfrow=plotdim,mar=c(2,4,1,2)+.1,lwd=.5,xpd=T, omi=c(0,0.2,0,0))
  #on.exit(par(opar))

  labs <- c(paste("Region",seq(nplt-1)),"WCPO")
   year <- B[,1]
  for(i in 1:nplt){
  dplt <- B[,i+1]/Bnof[,i+1] 
    plot(year,dplt,type='n',
         ylim=range(0,1),ann=F,axes=F,lwd=2,col=2)
    lines(year,dplt,lwd=2,lty=1)
    box(bty='l',lwd=.1)
    axis(2,lwd=.1,cex.axis=.8,las=1)
    if((i-nplt)>-plotdim[2]) axis(1,lwd=.1,cex.axis=.8) else axis(1,labels=F,lwd=.1)
    legend("topright",labs[i],cex=0.9,bty="n")
}
mtext(side=2, outer=T, text=textlab, line=0)
savePlot(paste(figdir,pnames[1],sep=""),ptype)
dev.off()

#WCPO plot
par(mfrow=c(1,1))
    plot(year,dplt,type='n',
         ylim=range(0,1),ann=F,axes=F,lwd=2,col=2)
    lines(year,dplt,lwd=2,lty=1)
    box(bty='l',lwd=.1)
    axis(2,lwd=.1,cex.axis=.8,las=1)
    axis(1,lwd=.1,cex.axis=.8)
    legend("topright",labs[i],cex=0.9,bty="n")
points(refyear,refpoint,pch=16,col="red",cex=2)
abline(h=0.2,lty=4,col="grey",lwd=2)
mtext(side=2, outer=T, text=textlab, line=-2)
savePlot(paste(figdir,pnames[2],sep=""),ptype)
dev.off()
}





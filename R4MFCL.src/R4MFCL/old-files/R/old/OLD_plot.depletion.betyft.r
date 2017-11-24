plot.depletion.betyft <- function(plotrep=read.rep(baserep),type="SSB",plotdim=c(5,2))
{
##-----------------------------------------------------------------------
# Tidied up a little SJH 16/7/2011
##-----------------------------------------------------------------------

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
  textlab <- "Proportion of total spawning potential"
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
    axis(2,lwd=.1,cex.axis=.7,las=1)
    if((i-nplt)>-plotdim[2]) axis(1,lwd=.1,cex.axis=.7) else axis(1,labels=F,lwd=.1)
    legend("topright",labs[i],cex=0.7,bty="n")
}
mtext(side=2, outer=T, text=textlab, line=0)
}





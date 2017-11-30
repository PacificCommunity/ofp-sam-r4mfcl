 plot.F.time <-
function(
plotdir="H:/rmfcl/test/figs/",
plotrep="C:/assessments/alb/2008/6_area/28.splitgr3/plot-08.par.rep",
inifile="C:/assessments/alb/2008/6_area/28.splitgr3/alb.ini",
plotname="plotFtime",
plottype="wmf",
COL=T)
{
# SJH 19/01/2009
# Plots annual F by year for adults and juveniles (as defined by the maturity ogive in the *.ini file
# 

#time steps
nyr <- plotrep$nTimes
#first year
year1 <- plotrep$Year1
#number of time steps per year
tsteps <- plotrep$nRecs.yr
#number regions
nreg <- plotrep$nReg
#number of age classes
nages <- plotrep$nAges
#number of fisheries
nfish <- plotrep$nFisheries
##fishery incidents
fish1 <- plotrep$nRlz.fsh
##fishery incidents times
fish2 <- plotrep$Rlz.t.fsh

Fm <- list()
Fj <- list()

year <- trunc(seq(year1,length=nyr,by=1/tsteps))


  ##=======================================================================
  ## Population number by age (across), year (or time pd) (down) and region
  ##=======================================================================
# agemat <- array(NA, c(nyr, nages, nreg))
agemat <- plotrep$NatYrAgeReg
 
 
  ##=======================================================================
  ## Fishing mortality by age (across), year (or time pd) (down) and region
  ##=======================================================================
# Fmat <- array(NA, c(nyr, nages, nreg))
Fmat <- plotrep$FatYrAgeReg


##
a <- apply(Fmat*agemat, c(1,2), sum)/apply(agemat,c(1,2), sum)
# maturity at age
mat <- scan(inifile, nlines=1, skip=3)

nadult <- t(t(apply(agemat,c(1,2), sum)) * mat) 
x <- apply(nadult*a,1,sum)/apply(nadult,1,sum)
Fm <- aggregate(x, list(year), sum)[,-1]
njuv <- t(t(apply(agemat,c(1,2), sum)) * 1-mat) 
x <- apply(njuv*a,1,sum)/apply(njuv,1,sum)
Fj <- aggregate(x, list(year), sum)[,-1]


year <- sort(unique(year))

#browser()
#    windows(7,4.5)
    dev.new(width=7,height=4.5)

    plot(year,Fm,type='l',ylim=range(0,Fm, Fj),ann=F,axes=F,lwd=2)
if(COL)
{
    lines(year,Fj,lwd=2,lty=2,col=2)
    legend(year[1],max(Fm,Fj),lty=1:2,lwd=2,col=1:2,legend=c("Fadult","Fjuvenile"),y.intersp=1.5,cex=.7)
}
else
{
    lines(year,Fj,lwd=2,lty=2,col=1)
    legend(year[1],max(Fm,Fj),lty=1:2,lwd=2,col=1,legend=c("Fadult","Fjuvenile"),y.intersp=1.5,cex=.7)
}
    #title(ylab="Fishing mortalilty (per year)")
    box(bty='l',lwd=1,cex.axis=.7)
    axis(2,lwd=1,cex.axis=0.8,las=1)
    axis(1,lwd=1,cex.axis=0.8)
    mtext(side=2, outer=T, "Annual fishing mortality", cex=1, line =-1.5)
    mtext(side=1, outer=T, "Year", cex=1, line =-2.5)
    
    savePlot(filename=paste(plotdir,plotname,sep=""),type=plottype)
dev.off()
}

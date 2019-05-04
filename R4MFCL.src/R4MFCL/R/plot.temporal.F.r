plot.temporal.F  <-  function(plotrep=read.rep(baserep), inifile=read.ini(baseini), dome=TRUE, ymax=NULL, French=FALSE)
{
# Plots annual F by year for adults and juveniles (as defined by the maturity ogive in the *.ini file
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

    # SJH 22/09/11 -- to assist with TFAR
    if(French)
    {
    leglab <- c("adulte","juv\u{E9}nile")
#    leglab <- c("adulte","juv\\'enile")
    myxlab <-  "Ann\u{E9}es"
#    myxlab <-  "Ann\\'ees"
    myylab <- "Taux annuel de mortalit\u{E9}"
#    myylab <- "Taux annuel de mortalit\\'e"
    }else
    {
    leglab <- c("adult","juvenile")
    myxlab <-  "Year"
    myylab <- "Annual fishing mortality"
    }

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
#mat <- scan(inifile, nlines=1, skip=3)
mat <- inifile$mat

# need to carry across from maximum for the maturity scheduled which is dome-shaped
  if(dome)
  {
  tmp <- rev(order(mat))[1] # find the maximum and make the rest also = 1
  mat[tmp:length(mat)] <- 1
  }

nadult <- t(t(apply(agemat,c(1,2), sum)) * mat)
x <- apply(nadult*a,1,sum)/apply(nadult,1,sum)
Fm <- aggregate(x, list(year), sum)[,-1]
njuv <- t(t(apply(agemat,c(1,2), sum)) * 1-mat)
x <- apply(njuv*a,1,sum)/apply(njuv,1,sum)
Fj <- aggregate(x, list(year), sum)[,-1]

year <- sort(unique(year))

    if(is.null(ymax)){
    ymax <- range(0,Fm, Fj)
    }
    else{
    ymax <- range(0,ymax)
    }

    plot(year,Fm,type='l',ylim=ymax,ann=F,axes=F,lwd=2)
    lines(year,Fj,lwd=2,lty=1,col=2)
    box()
    legend("topleft",lty=1,lwd=2,col=1:2,legend=leglab,y.intersp=1.5,cex=1.1,bty="n")
#}
#else
#{
#    lines(year,Fj,lwd=2,lty=2,col=1)
#    legend("topleft",lty=1:2,lwd=2,col=1,legend=c("Fadult","Fjuvenile"),y.intersp=1.5,cex=.7,bty="n")
#}
    #title(ylab="Fishing mortalilty (per year)")
    box(bty='l',lwd=1,cex.axis=.7)
    axis(2,lwd=1,cex.axis=0.8,las=1)
    axis(1,lwd=1,cex.axis=0.8)
    mtext(side=2, outer=T, myylab, cex=1, line =-1.5)
    mtext(side=1, outer=T, myxlab, cex=1, line =-2.5)

#    savePlot(filename=paste(plotdir,plotname,sep=""),type=plottype)
#dev.off()
}

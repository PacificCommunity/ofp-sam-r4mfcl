plot.Majuro <- function(outcome.obj=get.outcomes.2014(read.rep(baserep), read.par(basepar), catch.rep=basecat, nofish=T,
                        nofishp=c(44,4),lateyr=2012),
                        msy.obj=basemsy, plotdir=figdir, plotrep=baserep, current=F, latest=T,
                        plotname="SBF0Kobe", plottype="png", add.labels=T, kplotdim=c(1,2),
                        circ.cls = c("white","red"), green.zone=list(x=c(0.4,0.6,0.6,0.4,0.4), y=c(0,0,1,1,0), col="green", density=c(10,20)))
{
# trying to come up with a Kobe-like plot with the LRP on it 
# SJH 7/8/2014 8:34:29 PM

nyr <- plotrep$nTimes
year1 <- plotrep$Year1
tsteps <- plotrep$nRecs.yr
year <- trunc(seq(year1,length=nyr,by=1/tsteps))
# Needed for colours and other stuff - it appears that the last year is not plotted
years <- unique(year)


  B <- plotrep$AdultBiomass
  Bnof <- plotrep$AdultBiomass.nofish
  textlab <- "Proportion of total spawning potential - SB/SB(F=0)"

  B <- apply(B,1,sum)
  Bnof <- apply(Bnof,1,sum)
  Brat <- B/Bnof
  
  ##--- aggregate by year
  B <- aggregate(Brat,list(year),mean)

#  SBMSY <- outcome.obj$SBmsy / outcome.obj$SBF0

## grab the results from the plotrep

#      Brat <- msy.obj$SBSBmsy
      Bratcurr <- outcome.obj$SBcurr.SBF0
      Bratlate <- outcome.obj$SBlatest.SBF0

      Brat <- B[-(nrow(B)),2]  #exclude final year as we don't have an F/FMSY
      Frat <- msy.obj$FFmsy
#Frat <- msy.obj$FFmsy[-(length(msy.obj$FFmsy))]
      Fratcurr <- outcome.obj$Fcurr.Fmsy
#      browser()

# add template then lines
plot.Majuro.template(pdims=kplotdim,Green.Zone=green.zone)
      
      cols <- hsv(0.75,1:length(years)/length(years),0.8,1)
      if(length(Brat) == length(Frat))
      {
        points(Brat, Frat, col=cols, pch=16, cex=3)
        lines(Brat, Frat, lwd=1, col="black", lty=1)
      }
      if(!length(Brat) == length(Frat))
      {
        points(Brat, Frat[-length(Frat)], col=cols, pch=16, cex=3)
        lines(Brat, Frat[-length(Frat)], lwd=1, col="black", lty=1)
      }
      textcol<-"white"

          for (i in 1:(length(Frat)-1)){
                  arrows(Brat[i], Frat[i], Brat[i+1], Frat[i+1], angle=15, length=0.12)
          }

#add year labels

if(add.labels)
{
labels <- seq(1950,2010,by=5)
a <- match(labels,years)
          for(i in a){
          text(Brat[i],Frat[i], as.character(years[i]), cex=0.75, col=textcol)
          }
}
# large point for current
if(current) points(Bratcurr, Fratcurr, col="black",bg=circ.cls[1], pch=25, cex=2)
if(latest) points(Bratlate, Fratcurr, col="black",bg=circ.cls[2], pch=21, cex=2.5)
}


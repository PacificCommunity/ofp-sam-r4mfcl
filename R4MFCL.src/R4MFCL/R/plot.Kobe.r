plot.Kobe <- function(plotdir="S:\\OFP Publications\\Tuna Fishery Assessment Report\\2007\\Figures\\BET\\",
                      msy.obj=msy.obj.out,plotrep=baserep,
                      type="SSB",plotname="Kobe",plottype="wmf",add.labels=T,kplotdim=c(5,3),French=F,
                      outcome.obj=get.outcomes.2014(read.rep(baserep),read.par(basepar),catch.rep=basecat,nofish=T,nofishp=c(44,4),lateyr=2012),
                      circ.cls = c("white","red"))
{
# Updated 11/07/2014 9:55:01 AM SJH - circles for Bcurr/BMSY and Blatest/BMSY - no big black circle for terminal point
# You can define the circle colours

#template
      plot.Kobe.template(pdims=kplotdim,Type=type,FRENCH=French)
## assign key vars from the plotrep
#time steps
nyr <- plotrep$nTimes
#first year
year1 <- plotrep$Year1
#number of time steps per year
tsteps <- plotrep$nRecs.yr

year <- trunc(seq(year1,length=nyr,by=1/tsteps))
# Needed for colours and other stuff - it appears that the last year is not plotted
years <- unique(year)
#Don't include the most recent year
#years <- years[-length(years)]

## grab the results from the plotrep

      if(type=="SSB")
      {
      Brat <- msy.obj$SBSBmsy
      Bratcurr <- outcome.obj$SBrec.SBmsy
      Bratlate <- outcome.obj$SBlatest.SBmsy
      }
      else
      {
      Brat <- msy.obj$BBmsy
      Bratcurr <- outcome.obj$Brec.Bmsy
      Bratlate <- outcome.obj$Blatest.Bmsy
      }

      Frat <- msy.obj$FFmsy
      Fratcurr <- 1/plotrep$Fmult

      #browser()
      #if(COL)
      #{
      cols <- hsv(0.75,1:length(years)/length(years),0.8,1)
      points(Brat, Frat, col=cols, pch=16, cex=3)
      textcol<-"white"
      #}
      #else
      #{
      #points(Brat, Frat, pch=16, col="grey",cex=2)
      #textcol<-"black"
      #}

lines(Brat, Frat, lwd=1, col="black", lty=1)
#include lines as arrows
          for ( i in 1:(length(years)-1)){
                  arrows(Brat[i], Frat[i], Brat[i+1], Frat[i+1], angle=15, length=0.12)
          }

#add year labels
#labels <- 1950+1:10*5
if(add.labels)
{
labels <- seq(1950,2015,by=5)
a <- match(labels,years)
          for(i in a){
          text(Brat[i],Frat[i], as.character(years[i]), cex=0.75, col=textcol)
          }
}
# large point for current and latest
points(Bratcurr,Fratcurr, col="black",bg=circ.cls[1], pch=25, cex=2)
points(Bratlate,Fratcurr, col="black",bg=circ.cls[2], pch=21, cex=2.5)
#points(Brat[length(Brat)],Frat[length(Frat)], col="black", pch=16, cex=2)
}


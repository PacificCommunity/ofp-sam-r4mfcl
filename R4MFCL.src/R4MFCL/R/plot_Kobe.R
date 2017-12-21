plot_Kobe <- function(plotdir="S:/OFP Publications/Tuna Fishery Assessment Report/2007/Figures/BET/",plotrep=test,type="SSB",plotname="Kobe",plottype="wmf",COL=T) {
# Adds the time series to the template plot

#template
if(COL) {
  plot.Kobe.template.col()
} else {
  plot.Kobe.template.bw()
}

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
#years <- years[-length(years)]

## grab the results from the plotrep

if(type=="SSB") {
  b.bmsy <- plotrep$Eq.SB.SBmsy
} else {
  b.bmsy <- plotrep$Eq.B.Bmsy
}

# annual average for Bratio
Brat <- aggregate(b.bmsy,list(year),mean)[,-1]

### grab the results from the plotrep
f.fmsy <- plotrep$Eq.F.Fmsy
# annual average for Fratio
Frat <- aggregate(f.fmsy,list(year),mean)[,-1]

if(COL) {
  cols <- hsv(0.75,1:length(years)/length(years),0.8,1)
  points(Brat, Frat, col=cols, pch=16, cex=3)
  textcol<-"white"
} else {
  points(Brat, Frat, pch=16, col="grey",cex=2)
  textcol<-"black"
}

lines(Brat, Frat, lwd=1, col="black", lty=1)
#include lines as arrows
for ( i in 1:(length(years)-1)) {
  arrows(Brat[i], Frat[i], Brat[i+1], Frat[i+1], angle=15, length=0.12)
}

#plot last year
points(Brat[length(years)], Frat[length(years)], col=1, pch=16, cex=1.5)
#add year labels
#labels <- 1950+1:10*5
labels <- seq(1950,2010,by=5)
a <- match(labels,years)
for(i in a) {
  text(Brat[i],Frat[i], as.character(years[i]), cex=0.75, col=textcol)
}

savePlot(filename=paste(plotdir,plotname,sep=""),type=plottype)
dev.off()
}

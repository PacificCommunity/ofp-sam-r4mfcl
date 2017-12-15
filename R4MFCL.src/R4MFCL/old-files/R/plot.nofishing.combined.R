 plot.nofishing.combined <-
function(plotdir="H:/rmfcl/test/figs/",plotrep=testq0,type="SSB",plotname="H:/rmfcl/test/figs/Kobe",plottype="wmf",COL=T)
{
##-----------------------------------------------------------------------
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
  textlab <- "Adult biomass (1000s mt)"
}
else
{
  B <- plotrep$TotBiomass
  Bnof <- plotrep$TotalBiomass.nofish
  textlab <- "Total biomass (1000s mt)"
}

#browser()

  if(nreg==1){
  B <- as.vector(B/1000)
  Bnof <- as.vector(Bnof/1000)
  }
  else{
  B <- apply(B,1,sum) /1000
  Bnof <- apply(Bnof,1,sum)  /1000
  }
#browser()

  ##--- aggregate by year
  if(tsteps==1){
  B <- data.frame(year=year,B=B)
  Bnof <- data.frame(year=year,Bnof=Bnof)
  }
  else{
  B <- aggregate(B,list(year),mean)
  Bnof <- aggregate(Bnof,list(year),mean)
  }

#  ##-- add on totals  and divide by 1000
#  B <- apply(B,1,sum) /1000
#  Bnof <- apply(Bnof,1,sum)  /1000
#  ##--- aggregate by year
#  B <- aggregate(B,list(year),mean)
#  Bnof <- aggregate(Bnof,list(year),mean)




   year <- B[,1]

    windows(7,4.5)

    plot(year,Bnof[,2],type='n',
         ylim=range(0,B[,2],Bnof[,2]),ann=F,axes=F,lwd=2,col=2)
    lines(year,B[,2],lwd=2,lty=1)

if(COL)
{
    lines(year,Bnof[,2],lwd=2,lty=2,col=2)
}
else
{
    lines(year,Bnof[,2],lwd=2,lty=2,col=1)
}


    box(bty='l',lwd=1)
    axis(2,lwd=1,cex.axis=.8)
    axis(1,lwd=1,cex.axis=.8)



if(COL)
{
  legend(year1+5,mean(c(0,max(Bnof[,2])))/2,lty=1:2,lwd=2,col=1:2,cex=.7,
         legend=c("Fished biomass","Unfished biomass"),y.intersp=1.5)
}
else
{
  legend(year1+5,mean(c(0,max(Bnof[,2])))/2,lty=1:2,lwd=2,col=1,cex=.7,
         legend=c("Fished biomass","Unfished biomass"),y.intersp=1.5)
}
mtext(side=2, outer=T, text=textlab, line=-1.5)
mtext(side=1, outer=T, text="Year", line=-2.5)
savePlot(filename=paste(plotdir,plotname,sep=""),type=plottype)
dev.off()
}

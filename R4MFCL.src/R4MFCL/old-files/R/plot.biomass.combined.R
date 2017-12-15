 plot.biomass.combined <-
function(plotdir="H:/rmfcl/test/figs/",plotrep=test,
varfile=NULL,
type="SSB",
plotname="H:/rmfcl/test/figs/biomass",
plottype="wmf")
{
##-----------------------------------------------------------------------
# Based on original by PK
# SJH 20/1/2009
# Plots biomass combined across all regions
# Option to add on CI as a polygoon
#
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
  textlab <- "Adult biomass (1000s mt)"
}
else
{
  B <- plotrep$TotBiomass
  textlab <- "Total biomass (1000s mt)"
}
if(is.null(varfile))
{
}
else
{
    if(type=="SSB")
    {
      Btot <- varfile$adult_rbio
    }
    else
    {
      Btot <- varfile$rbio
      }
B.ub <- exp(Btot[,2]+2*Btot[,1]) /1000000
B.lb <- exp(Btot[,2]-2*Btot[,1]) /1000000

# now get them into the same format as the B
        ##--- aggregate by year
        if(nreg==1){
        B.ub <- data.frame(year=year,B=B.ub)
        B.lb <- data.frame(year=year,B=B.lb)
        }
        else{
        B.ub <- aggregate(B.ub,list(year),mean)
        B.lb <- aggregate(B.lb,list(year),mean)
        }
}

#browser()
  ##-- add on totals  and divide by 1000
  if(nreg==1){
  B <- as.vector(B/1000)
  }
  else{
  B <- apply(B,1,sum) /1000
  }
#browser()

  ##--- aggregate by year
  if(tsteps==1){
  B <- data.frame(year=year,B=B)
  }
  else{
  B <- aggregate(B,list(year),mean)
  }

  labs <- "WCPO"
  year <- B[,1]
#browser()

    if(is.null(varfile)){
    y.ub <- max(c(0,B[,2]))
    }
    else{
    y.ub <- max(c(0,B.ub[,2]))
    }

    windows(7,4.5)

    plot(year,B[,2],type='n',
         ylim=range(0,y.ub),ann=F,axes=F,lwd=2,col=2,xlab="Year")
# Add the CI if necessary
    if(is.null(varfile)){
    }
    else{
    polygon(c(year,rev(year)),c(B.ub[,2],rev(B.lb[,2])),
            border="white",col="LightSlateGrey")
    }
    lines(year,B[,2],lwd=2,lty=1)
    box(bty='l',lwd=1)
    axis(2,lwd=1,cex.axis=.85)
    axis(1,lwd=1,cex.axis=.85)
mtext(side=2, outer=T, text=textlab, line=-1.5)
mtext(side=1, outer=T, text="Year", line=-2.5)
savePlot(filename=paste(plotdir,plotname,sep=""),type=plottype)
dev.off()
}

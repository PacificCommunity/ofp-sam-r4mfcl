 plot.recruitment.combined <-
function(plotdir="H:/rmfcl/test/figs/",plotrep=test,varfile=NULL,plotname="H:/rmfcl/test/figs/recruitment_combined",plottype="wmf")
{
##-----------------------------------------------------------------------
# SJH 20/1/2009
# Plots biomass combined across all regions
# Option to add on CI as a polygoon
# 6/2/09 - updated to handle single region and / or single time step models
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

R <- plotrep$Recruitment

if(is.null(varfile))
{
}
else
{
Rtot <- varfile$ln_abs_recr
R.ub <- exp(Rtot[,2]+2*Rtot[,1]) /1000000
R.lb <- exp(Rtot[,2]-2*Rtot[,1]) /1000000
# now get them into the same format as the B
##--- aggregate by year
R.ub <- aggregate(R.ub,list(year),sum)
R.lb <- aggregate(R.lb,list(year),sum)
}

#browser()

  ##-- add on totals  and divide by 1000
  if(nreg==1){
  R <- as.vector(R/1000000)
  }
  else
  {
  R <- apply(R,1,sum) /1000000
  }
#browser()

  ##--- aggregate by year
  if(tsteps==1){
  R <- data.frame(year=year,R=R)
  }
  else
  {
  R <- aggregate(R,list(year),sum)
  }
  labs <- "WCPO"
  year <- R[,1]

  #browser()

    if(is.null(varfile))
    {
    y.ub <- max(c(0,R[,2]))
    }
    else
    {
    y.ub <- max(c(0,R.ub[,2]))
    }

    windows(7,4.5)

    plot(year,R[,2],type='n',
         ylim=range(0,y.ub),ann=F,axes=F,lwd=2,col=2)
# Add the CI if necessary
    if(is.null(varfile))
    {
    }
    else
    {
    polygon(c(year,rev(year)),c(R.ub[,2],rev(R.lb[,2])),
            border="white",col="LightSlateGrey")
    }
    lines(year,R[,2],lwd=2,lty=1)
    box(bty='l',lwd=1)
    axis(2,lwd=1,cex.axis=.8)
    axis(1,lwd=1,cex.axis=.8)
mtext(side=2, outer=T, text="Recruitment (millions)", line=-1.5)
mtext(side=1, outer=T, text="Year", line=-2.5)
savePlot(filename=paste(plotdir,plotname,sep=""),type=plottype)
dev.off()
}

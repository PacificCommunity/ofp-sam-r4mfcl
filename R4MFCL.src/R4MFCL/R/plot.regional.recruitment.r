plot.regional.recruitment <- function(plotrep=read(baserep), varfile=NULL, plot.layout=c(5, 2), legpos="topright")
{
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
  
  # reading in the numbers from plotrep
  R <- plotrep$Recruitment
  
  # read bounds from varfile for combined R
  if(is.null(varfile))
  {
  }
  else
  {
    Rtot <- varfile$ln_abs_recr
    R.ub <- exp(Rtot[,2]+2*Rtot[,1]) /1000000
    R.lb <- exp(Rtot[,2]-2*Rtot[,1]) /1000000
    # now get them into the same format as the B
    R.ub <- aggregate(R.ub,list(year),sum)
    R.lb <- aggregate(R.lb,list(year),sum)
  }
  
  ##-- add on totals  and divide by 1000000
  R <- cbind(R,apply(R,1,sum)) /1000000
  ##--- aggregate by year
  R <- aggregate(R,list(year),sum)
  
  nplt <- nreg+1
  opar <- par(mfrow=plot.layout,mar=c(2,4,1,2)+.1,lwd=.5,xpd=T, omi=c(0,0.2,0,0))
  on.exit(par(opar))
  
  labs <- c(paste("Region",seq(nplt-1)),"Overall")
  year <- R[,1]
  
  for(i in 1:nplt){
    
    if(i!=nplt)
    {
      plot(year,R[,i+1],type='n',
           ylim=range(0,R[,i+1]),ann=F,axes=F,lwd=2,col=2)
      lines(year,R[,i+1],lwd=2,lty=1)
      box()
      axis(2,lwd=.1,cex.axis=1.2,las=1)
    }
    else
    {
      # Specific code for WCPO plot ..... and klunky to allow for missing var file
      if(is.null(varfile))
      {
        y.ub <- max(c(0,R[,i+1]))
      }
      else
      {
        y.ub <- max(c(0,R.ub[,2]))
      }
      
      
      plot(year,R[,i+1],type='n',
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
      lines(year,R[,i+1],lwd=2,lty=1)
      box()
      axis(2,lwd=.1,cex.axis=1.2,las=1)
    }
    if(i==nreg | i==nplt) axis(1,lwd=.1,cex.axis=1.2) else axis(1,labels=F,lwd=.1)
    legend(legpos,legend=labs[i],cex=1.2,bty="n")
  }
  mtext(side=2, outer=T, text="Recruitment (millions)", line=0)
}

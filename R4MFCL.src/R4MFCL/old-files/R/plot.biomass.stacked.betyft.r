plot.biomass.stacked.betyft <- function(plotrep=read.rep(baserep),pmain="Run 3d",type="SSB",maxylim=NULL,sp="skj",specs=regionspecs)
{
# SJH 23/07/2010
# Biomass by region

nyr <- plotrep$nTimes
#first year
year1 <- plotrep$Year1
#number of time steps per year
tsteps <- plotrep$nRecs.yr
year <- trunc(seq(year1,length=nyr,by=1/tsteps))

        if(type=="SSB")
        {
         B <- plotrep$AdultBiomass/1000
         textlab <- "Spawning potential"
        }
        else
        {
        B <- plotrep$TotBiomass/1000
        textlab <- "Total biomass (1000s mt)"
        }
    ##--- aggregate by year
Bout <- aggregate(B,list(year),mean)

#browser()
titles <- paste("Region",seq(1,(ncol(Bout)-1)))
    if(is.null(maxylim))
    {
    maxylim <- max(apply(Bout[,2:ncol(Bout)],1,sum))
    }
yr <- Bout[,1]

cols <- specs$cls[specs[[paste(sp,"col",sep="")]]]
#cols <- c("blue","darkred","green","orange","pink","yellow","grey","lightblue","red")

windows(12,8)
par(mfrow=c(1,1))
  plot(1,1,ylim=c(0,maxylim),xlim=range(Bout[,1]), type="n", xlab="", ylab="",las=1)
  a <- c(yr, rev(yr))
  b <- rep(0, length(yr))
  d <- Bout[,2]
  polygon(a, c(b,rev(d)), col=cols[1])
  for(i in 3:ncol(Bout)){
    b <- d
    d <- d+ Bout[,i]
    polygon(a, c(b,rev(d)), col=cols[i-1])
    }
legend("topright",maxylim,legend=titles,fill=cols,bty="n")
mtext(1,outer=T,text="Year",line=-2.5)
mtext(2,outer=T,text=textlab,line=-1.2)

        if(!is.null(pmain))
        {
        title(main=pmain)
        }
}
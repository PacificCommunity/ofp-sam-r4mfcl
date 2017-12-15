plot.biomass.compare.betyft<- function(legpos="topright",
repfiles=list(
read.rep("C:/condor_work/runs/run10/plot-11.par.rep"),
read.rep("C:/condor_work/runs/run4/plot-11.par.rep"),
read.rep("C:/condor_work/runs/run7/plot-11.par.rep")),
modlab = NULL,type="SSB",cls=NULL)
{
if(is.null(modlab)) {modlab <- paste("model",seq(1,length(repfiles))) }

#nyr <- repfiles[[1]]$nTimes
##first year
#year1 <- repfiles[[1]]$Year1
##number of time steps per year
#tsteps <- repfiles[[1]]$nRecs.yr
#year <- trunc(seq(year1,length=nyr,by=1/tsteps))
    windows(12, 8)
    par(mfrow=c(1,1),oma=c(1,2,1,1))

    maxy <- c()
    recr <- list()
    for(i in 1:length(repfiles))
    {

nyr <- repfiles[[i]]$nTimes
#first year
year1 <- repfiles[[i]]$Year1
#number of time steps per year
tsteps <- repfiles[[i]]$nRecs.yr
year <- trunc(seq(year1,length=nyr,by=1/tsteps))

        if(type=="SSB")
        {
         B <- apply(repfiles[[i]]$AdultBiomass,1,sum)/1000
         textlab <- "Spawning potential"
        }
        else
        {
        B <- apply(repfiles[[i]]$TotBiomass,1,sum)/1000
        textlab <- "Total biomass (1000s mt)"
        }
    ##--- aggregate by year
    recr[[i]] <- aggregate(B,list(year),mean)
    maxy <- c(maxy,max(recr[[i]][,2]))
    }

#    years2 <- seq(min(year),max(year),by=1)
    par(las=0)
#    plot(years2, recr[[i]][,2], xlim=c(1950,2010), ylim=c(0,max(maxy)*1.1), ylab="", xlab="", type="l")
    plot(recr[[1]][,1],recr[[1]][,2], ylim=c(0,max(maxy)*1.1), ylab="", xlab="", type="n",las=1)

    for(i in 2:length(recr)){
	   if(is.null(cls)) lines(recr[[i]][,1],recr[[i]][,2], lwd=2, col=i) else lines(recr[[i]][,1],recr[[i]][,2], lwd=2, col=cls[i])
    }
    # ref case last
    if(is.null(cls)) lines(recr[[1]][,1], recr[[1]][,2], lwd=3, col=1)  else lines(recr[[1]][,1], recr[[1]][,2], lwd=3, col=cls[1])

    if(is.null(cls)) legend(legpos, lwd=2, col=c(1:length(recr)), lty=1, legend=modlab,bty="n") else legend(legpos, lwd=2, col=cls, lty=1, legend=modlab,bty="n")
mtext(side=2, text=textlab, line=3.5)
}



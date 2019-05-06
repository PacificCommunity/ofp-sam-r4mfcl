plot.biomass.compare <- function(legpos="topright",
                                       repfiles=list(
                                       read.rep("C:/condor_work/runs/run10/plot-11.par.rep"),
                                       read.rep("C:/condor_work/runs/run4/plot-11.par.rep"),
                                       read.rep("C:/condor_work/runs/run7/plot-11.par.rep")),
                                       modlab = NULL,type="SSB",cls=NULL,regions = c("multi","single","single"),
                                       aggyrs = c(TRUE,FALSE,FALSE), SBlab = "Spawning potential",
                                       xlimits = c(1972,2015), ymult = 1.1)
{
if(is.null(modlab)) {modlab <- paste("model",seq(1,length(repfiles))) }

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
            if(regions[i] == "multi") B <- apply(repfiles[[i]]$AdultBiomass,1,sum)/1000
            if(regions[i] == "single") B <- repfiles[[i]]$AdultBiomass/1000
         
            textlab <- paste(SBlab, "(1000's mt)")
        }
        else
        {
            B <- apply(repfiles[[i]]$TotBiomass,1,sum)/1000
            textlab <- "Total biomass (1000's mt)"
        }
    ##--- aggregate by year
    if(aggyrs[i])  recr[[i]] <- aggregate(B,list(year),mean)
    if(!aggyrs[i]) recr[[i]] <- cbind(as.numeric(year),as.numeric(B))
    maxy <- c(maxy,max(recr[[i]][,2]))
    }

#    years2 <- seq(min(year),max(year),by=1)
    par(las=0)
#    plot(years2, recr[[i]][,2], xlim=c(1950,2010), ylim=c(0,max(maxy)*1.1), ylab="", xlab="", type="l")
    plot(recr[[1]][,1],recr[[1]][,2], xlim=xlimits, ylim=c(0,max(maxy)*ymult), ylab="", xlab="", 
       type="n",las=1)

    for(i in 2:length(recr)){
      if(is.null(cls)) mycol <- i else mycol <- cls[i] 

	    lines(recr[[i]][,1],recr[[i]][,2], lwd=2, col=mycol)
    }
# Plot reference case last
    if(is.null(cls)) mycol <- 1 else mycol <- cls[1] 
	  lines(recr[[1]][,1],recr[[1]][,2], lwd=2, col=mycol)

    if(is.null(cls)) mycol <- c(1:length(recr)) else mycol <- cls 

    legend(legpos, lwd=2, col=mycol, lty=1, legend=modlab,bty="n")
    mtext(side=2, text=textlab, line=3.5)
}



plot.depletion.compare <- function(legpos="topright",
                                   repfiles=list(read.rep("C:/condor_work/runs/run10/plot-11.par.rep"),
                                                 read.rep("C:/condor_work/runs/run4/plot-11.par.rep"),
                                                 read.rep("C:/condor_work/runs/run7/plot-11.par.rep")),
                                   modlab = NULL, type="SSB",cls=NULL,xlimits=c(1972,2015))
{
# SJH 9/7/2015 - hack from biomass compare
    if(is.null(modlab)) {modlab <- paste("model",seq(1,length(repfiles))) }

    ## par(mfrow=c(1,1),oma=c(1,2,1,1))

    maxy <- c()
    recr <- list()

    for(i in 1:length(repfiles))
    {

      nyr <- repfiles[[i]]$nTimes
    #first year
      year1 <- repfiles[[i]]$Year1
    #number of time steps per year
      tsteps <- repfiles[[i]]$nRecs.yr
        ## year <- trunc(seq(year1,length=nyr,by=1/tsteps))
        year <- seq(year1,length=nyr,by=1/tsteps)
      nregion <- repfiles[[i]]$nReg

    if(type=="SSB")
    {
      if(!nregion == 1)
      {
          B <- apply(repfiles[[i]]$AdultBiomass,1,sum)
          Bnof <- apply(repfiles[[i]]$AdultBiomass.nofish,1,sum)
      } else {
          B <- repfiles[[i]]$AdultBiomass
          Bnof <- repfiles[[i]]$AdultBiomass.nofish
      }
      textlab <- expression("Estimate of depletion "*phantom(10)*"SB  / SB "[" F=0"])
    }
    else
    {
      if(!nregion == 1)
      {
        B <- apply(repfiles[[i]]$TotBiomass,1,sum)
        Bnof <- apply(repfiles[[i]]$TotalBiomass.nofish,1,sum)
      } else {
        B <- repfiles[[i]]$TotBiomass
        Bnof <- repfiles[[i]]$TotalBiomass.nofish
      }
      textlab <- "Proportion of total biomass"
    }

    dplt <- B/Bnof
      ## if(tsteps == 1){
        recr[[i]] <- cbind(year,as.vector(dplt))
      ## } else {
      ##   recr[[i]] <- aggregate(dplt,list(year),mean)
      ## }
    }

    par(las=0)
    plot(recr[[1]][,1],recr[[1]][,2], xlim = xlimits, ylim=c(0,1), ylab="", xlab="", type="n",las=1)

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



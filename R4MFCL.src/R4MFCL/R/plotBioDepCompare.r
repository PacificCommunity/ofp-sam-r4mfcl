plotBioDepCompare <- function(legpos="topright",
                                       repfiles,
                                       modlab = NULL,type="SSB",cls=NULL,
                                       aggyrs = c(TRUE,FALSE,FALSE), SBlab = "Spawning potential",
                                       xlimits = c(1972,2018), ymult = 1,pngname=NULL,deplines=NULL)
{


## From depletion compare
    if(is.null(modlab)) {modlab <- paste("model",seq(1,length(repfiles))) }

    ## par(mfrow=c(1,1),oma=c(1,2,1,1))

    maxy <- c()
    depPlot <- list()
    bioPlot <- list()
    for(i in 1:length(repfiles))
    {

        nyr <- repfiles[[i]]$nTimes
                                        #first year
        year1 <- repfiles[[i]]$Year1
                                        #number of time steps per year
        tsteps <- repfiles[[i]]$nRecs.yr
        year <- trunc(seq(year1,length=nyr,by=1/tsteps))
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
            textlab <- "Estimate of depletion [SB/SB(F=0)]"
            textlab2 <- paste(SBlab, "(1000's mt)")
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
        if(tsteps == 1){
            depPlot[[i]] <- cbind(year,as.vector(dplt))
            bioPlot[[i]] <- cbind(as.numeric(year),as.numeric(B)/1000)
        } else {
            depPlot[[i]] <- aggregate(dplt,list(year),mean)
            bioPlot[[i]] <- aggregate(B/1000,list(year),mean)
        }
        maxy <- c(maxy,max(bioPlot[[i]][,2]))
    }
    if(!is.null(pngname)){               #If not requested to save assume that you have set up the par outside like you want
        png(filename=pngname,width=15,height=19,units='cm',res=700)
        par(mfcol=c(2,1),mar=c(2,5,1,0.5))
    }

    par(las=0)
    plot(depPlot[[1]][,1],depPlot[[1]][,2], xlim = xlimits, ylim=c(0,1), ylab="", xlab="", type="n",las=1)

    for(i in 2:length(depPlot)){
      if(is.null(cls)) mycol <- i else mycol <- cls[i]

	    lines(depPlot[[i]][,1],depPlot[[i]][,2], lwd=2, col=mycol)
    }
# Plot reference case last
    if(is.null(cls)) mycol <- 1 else mycol <- cls[1]
	  lines(depPlot[[1]][,1],depPlot[[1]][,2], lwd=2, col=mycol)

    if(is.null(cls)) mycol <- c(1:length(depPlot)) else mycol <- cls
    if(!is.null(deplines)) {abline(h=deplines[,1],col=deplines[,2])}
    legend(legpos, lwd=2, col=mycol, lty=1, legend=modlab,bty="n")
    mtext(side=2, text=textlab, line=3.5)

    par(las=0)
    plot(bioPlot[[1]][,1],bioPlot[[1]][,2], xlim=xlimits, ylim=c(0,max(maxy)*ymult), ylab="", xlab="",
       type="n",las=1)

    for(i in 2:length(bioPlot)){
      if(is.null(cls)) mycol <- i else mycol <- cls[i]

	    lines(bioPlot[[i]][,1],bioPlot[[i]][,2], lwd=2, col=mycol)
    }
# Plot reference case last
    if(is.null(cls)) mycol <- 1 else mycol <- cls[1]
	  lines(bioPlot[[1]][,1],bioPlot[[1]][,2], lwd=2, col=mycol)

    if(is.null(cls)) mycol <- c(1:length(bioPlot)) else mycol <- cls

    #legend(legpos[2], lwd=2, col=mycol, lty=1, legend=modlab,bty="n")
mtext(side=2, text=textlab2, line=3.5)

if(!is.null(pngname)){ dev.off()}

}

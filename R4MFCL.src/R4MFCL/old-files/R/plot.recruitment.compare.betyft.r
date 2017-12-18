plot.recruitment.compare.betyft <- function(legpos="topleft",
repfiles=list(
read.rep("C:/condor_work/runs/run10/plot-11.par.rep"),
read.rep("C:/condor_work/runs/run4/plot-11.par.rep"),
read.rep("C:/condor_work/runs/run7/plot-11.par.rep")),
modlab = NULL,add.pch=F,cls=NULL,tim.scale="yr")
{
# does both the plot and chucks out an object with the recruitment ratios
# which is important for BET


if(is.null(modlab)) {modlab <- paste("model",seq(1,length(repfiles))) }
nyr <- repfiles[[1]]$nTimes
year1 <- repfiles[[1]]$Year1
tsteps <- repfiles[[1]]$nRecs.yr
year <- trunc(seq(year1,length=nyr,by=1/tsteps))
year.qtr <- seq(year1+0.125,length=nyr,by=0.25)

    windows(12,8)
    par(mfrow=c(1,1),oma=c(1,2,1,1))

    maxy <- c()
    rangex <- c()
    recr <- list()
    for(i in 1:length(repfiles))
    {
    nyr <- repfiles[[i]]$nTimes
    year1 <- repfiles[[i]]$Year1
    tsteps <- repfiles[[i]]$nRecs.yr
    year <- trunc(seq(year1,length=nyr,by=1/tsteps))

    #browser()
    R <- apply(repfiles[[i]]$Recruitment,1,sum) /1000000
    if(tim.scale == "yrqtr")
    {
        recr[[i]] <- data.frame(year.qtr,R)
    } else
    {
        recr[[i]] <- aggregate(R,list(year),sum)
    }
    maxy <- c(maxy,max(recr[[i]][,2]))
    rangex <- c(rangex,range(recr[[i]][,1]))
    }
    #browser()
    #years2 <- lapply(recrs[[]][,1],range)
    #eq(min(year),max(year),by=1)
    plot(recr[[i]][,1], recr[[i]][,2], las=1,xlim=range(rangex), ylim=c(0,max(maxy)*1.1), ylab="", xlab="", type="n")

    for(i in 2:length(recr))
    {
           
           if(is.null(cls)) mycol <- i else mycol <- cls[i] 
           
           lines(recr[[i]][,1], recr[[i]][,2], lwd=2, col=mycol)
           if(add.pch) lines(recr[[i]][,1], recr[[i]][,2], lwd=2, col=mycol,type="b",pch=16)
    }
    # do the pass case last
    if(is.null(cls)) mycol <- 1 else mycol <- cls[1] 

    lines(recr[[1]][,1], recr[[1]][,2], lwd=3, col=mycol)
    if(add.pch) lines(recr[[i]][,1], recr[[i]][,2], lwd=2, col=mycol,type="b",pch=16)

    legend(legpos, lwd=2, col=c(1:length(recr)), lty=1, legend=modlab,bty="n")
    if(is.null(cls)) legend(legpos, lwd=2, col=c(1:length(recr)), lty=1, legend=modlab,bty="n") else legend(legpos, lwd=2, col=cls, lty=1, legend=modlab,bty="n")

    mtext(side=2, "Recruitment (millions of fish)", line=3,las=3)
}

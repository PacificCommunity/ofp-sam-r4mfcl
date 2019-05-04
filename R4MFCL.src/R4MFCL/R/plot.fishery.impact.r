plot.fishery.impact <- function(plotdir=figdir,
                                type = "Total",
                                plotrep = read.rep(baserep),
                                impfiles = NULL,
                                impnames = NULL,
                                plotname="plotimpact",
                                plottype="wmf",
                                yearmin=1960,
                                specs=gearspecs,
                                mynrows=5,
                                dev.size=list(c(1800, 2200),c(2000, 1600)))
{
#SJH 21/01/2009
# Does the fishery impact plot by taking the output straight from the plot.rep files
# You need to give the file names for the input files
## SJH 17/7/2011 - a kludge to cut-off the early years as I can't sort out the problem yet!
## SJH 29/7/2011 - make colours consistent
## SJH 11/8/2011 - flexible number of regions
## SJH 6/27/2014 5:19:56 PM - 2014 upgrade
  
    if(type=="Total")
    {
        fish.lab <- "TotBiomass"
        nofish.lab <- "TotalBiomass.nofish"
    } else {
        fish.lab <- "AdultBiomass"
        nofish.lab <- "AdultBiomass.nofish"
    }

    lnames <- specs$gearEN[match(impnames,specs$code)]
    cols <- specs$cls[match(impnames,specs$code)]


# Dimensioning stuff
###############################
# Rep file with dimensions
  tmp <- plotrep
#time steps
  nyr <- tmp$nTimes
#first year
  year1 <- tmp$Year1
#number of time steps per year
  tsteps <- tmp$nRecs.yr
#year <- trunc(seq(year1,length=nyr,by=1/tsteps))
  year <- trunc(seq(year1,length=nyr,by=1/tsteps))
  regs <- tmp$nReg
# Aget all the major input files
###############################
  fish <- tmp[fish.lab]
  nofish <- tmp[nofish.lab]

  for(i in 1:length(impnames))
  {
      data <- impfiles[[i]][nofish.lab]
      data <- cbind(data[[1]],apply(data[[1]],1,sum))
      data <- aggregate(data, list(year), mean)
      assign(paste(impnames[i],".ann",sep=""),data)
  }

# Do same calcs for fished and unfished
  x <- c("fish","nofish")
  for(i in 1:length(x))
  {
      data <- get(x[i])
      data <- cbind(data[[1]],apply(data[[1]],1,sum))
      data <- aggregate(data, list(year), mean)
      assign(paste(x[i],".ann",sep=""),data)
  }

# get gear-specific impacts
# keep a sum of total impact for use in next step
  all.imp <- matrix(0,nrow=nrow(fish.ann),ncol=ncol(fish.ann))

  for(i in 1:length(impnames))
  {
      data <- get(paste(impnames[i],".ann",sep=""))
      data <- data[,2:ncol(data)]-fish.ann[,2:ncol(fish.ann)]
      all.imp <- all.imp + data
      assign(paste(impnames[i],".imp",sep=""),data)
  }

# now do it on a regional basis
# Array by time, fishery, and region (with total)
  reg.imp <- array(NA, c(nrow(fish.ann), length(impnames), ncol(fish.ann)-1))
# get gear-specific impacts
  for(i in 1:length(impnames))
  {
      data <- get(paste(impnames[i],".imp",sep=""))
      reg.imp[,i,] <- as.matrix((data/all.imp)*((nofish.ann[,2:ncol(nofish.ann)]-fish.ann[,2:ncol(fish.ann)])/nofish.ann[,2:ncol(fish.ann)])*100)
  }
  reg.imp[is.na(reg.imp)] <- 0

# Lazy reassigning to avoid making changes to the plotting code
# Note that there is some five fishery / six region hardwiring there
  mat <- reg.imp
  labs <- impnames
  titles <- c(paste(rep("Region ", regs), 1:regs), "Overall")
  yr <- unique(year)
  
  den <- rep(NULL,5)
  ang <- rep(NA,5)

  windows(dev.size[[1]][1], dev.size[[1]][2])
  par(mfrow=c(mynrows,2), mar=c(2,3,1,2), omi=c(0.2,0.3,0,0))

  for (k in 1:(regs+1)){
      maxylim <- apply(mat[,1:length(labs),k], 1, sum)
      plot(yr, maxylim,ylim=c(0,100),xlim=c(yearmin,max(yr)), type="n",xlab="", ylab="",las=1)
      a <- c(yr, rev(yr))
      b <- rep(0, length(yr))
      d <- mat[,1,k]
      polygon(a, c(b,rev(d)), col=cols[1])
      
      for(i in 2:length(labs)){
          b <- d
          d <- d+ mat[,i,k]
          polygon(a, c(b,rev(d)), col=cols[i], density = den[i], angle = ang[i])
      }

      legend("top",legend=titles[k],bty="n",cex=1)
      if(k==(regs+1)) legend("topleft", legend=lnames,cex=0.8, pch=15,col=cols,bty="n")
  }
  mtext(side=2, line=1, outer =T, "Impact %")
  savePlot(filename=paste(plotdir,paste(plotname, type, ".", plottype, sep=""),sep=""),type=plottype)
  dev.off()

# Do a separate plot for the whole area
  windows(dev.size[[2]][1], dev.size[[2]][2])
  par(mfrow=c(1,1))

  for (k in (regs+1)){
      maxylim <- apply(mat[,1:length(labs),k], 1, sum)
      plot(yr, maxylim,ylim=c(0,100), xlim=c(yearmin,max(yr)),type="n", xlab="", ylab="",las=1)
      a <- c(yr, rev(yr))
      b <- rep(0, length(yr))
      d <- mat[,1,k]
      polygon(a, c(b,rev(d)), col=cols[1])
      
      for(i in 2:length(labs)){
          b <- d
          d <- d+ mat[,i,k]
          polygon(a, c(b,rev(d)), col=cols[i], density = den[i], angle = ang[i])
      }
  }

  legend("topleft", legend=lnames,cex=1, pch=15,col=cols,bty="n")
  mtext(side=2,outer=T,text="Impact %",cex=1,line=-1.5)
  mtext(side=1,outer=T,text="Year",cex=1,line=-2.5)
  savePlot(filename=paste(plotdir,paste(plotname,type,"WCPO",sep=""), ".", plottype, sep=""),type=plottype)
  dev.off()
}
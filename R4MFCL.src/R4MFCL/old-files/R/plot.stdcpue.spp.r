plot.stdcpue.spp <- function(repfiles=list(
read.rep("C:/condor_work/runs/run10/plot-11.par.rep"),
read.rep("C:/condor_work/runs/run4/plot-11.par.rep"),
read.rep("C:/condor_work/runs/run7/plot-11.par.rep")),
frqfiles=list(read.frq("C:/condor_work/runs/run10/bet.frq"),
read.frq("C:/condor_work/runs/run4/bet.frq"),
read.frq("C:/condor_work/runs/run7/bet.frq")),
modlab = NULL, fleetlabs=BET_fleets$fnames, nfish=LLall, N.cols=3, maxX=2013, maxY=4)
{
##-----------------------------------------------------------------------
##-----------------------------------------------------------------------
# SJH 08/07/09 - uses a read in rep file
# Updated 24/7/10 to read in a list
# Updated 20/7/11 to re-normalise all indices to an average of 1
# Updated 1/8/2011 to exclude missing effort
if(is.null(modlab)) {modlab <- paste("model",seq(1,length(repfiles))) }


  year.tmp <- list()
  cp.tmp <- list()

  for(i in 1:length(repfiles))
  {
      #cp.tmp[[i]] <- repfiles[[i]]$ObsCPUE /mean(repfiles[[i]]$ObsCPUE,na.rm=T)
      cp.tmp[[i]] <- repfiles[[i]]$ObsCPUE
      year.tmp[[i]] <- repfiles[[i]]$Rlz.t.fsh
  }

  plotwidth = ifelse(N.cols==3, 15, 12)
  windows(plotwidth, 12)
  par(mfrow=c(3,N.cols),mar=c(3,3,1.5,2)+.1)
  labs <- fleetlabs[nfish]

           for(i in 1:length(nfish))
           {
           cp <- list()

                  for(k in 1:length(repfiles))
                  {
                      effort <- frqfiles[[k]]$mat[,6][frqfiles[[k]]$mat[,4]==nfish[i]]
                      cp.tmp[[k]][nfish[i],][effort==-1] <-  NA
                      cp.tmp[[k]][nfish[i],] <- cp.tmp[[k]][nfish[i],]/mean(cp.tmp[[k]][nfish[i],],na.rm=T)

                      year <- trunc(year.tmp[[k]][nfish[i],!is.na(year.tmp[[k]][nfish[i],])])
                      cp[[k]] <- aggregate(cp.tmp[[k]][nfish[i],!is.na(year.tmp[[k]][nfish[i],])],list(year),mean,na.rm=T)      # !is.na(year.tmp  - matches Rlz.t.fsh to the length of Obs.cpue
                  }

              plot(cp[[1]][,1], cp[[1]][,2], las=1, xlim=range(c(1950,maxX)), ylim=c(0,maxY), ylab="", xlab="", type="n",cex.axis=1.2)

              if(length(cp) > 1)
              {
                  for(k in 2:length(cp))
                  {
                      lines(cp[[k]][,1], cp[[k]][,2], lwd=1, col=k)
                  }
              }
              
              # do the base case last
              lines(cp[[1]][,1], cp[[1]][,2], lwd=2, col=1)
              
              if(i == 1)
              {
                  legend("topright",legend=modlab,fill=1:length(repfiles),bty="n",cex=1)
              }

                title(main=paste("Region",i))

          }
}

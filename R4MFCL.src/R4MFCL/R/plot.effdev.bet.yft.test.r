plot.effdev.betyft.test <- function(plotrepfile=read.rep(baserep),frqfile=read.frq(basefrq),fleetlabs=BET_fleets$fnames,ylimit=c(-2,2),
                                    fishplot=c(1:33)[-LLall]){
##-----------------------------------------------------------------------
##-----------------------------------------------------------------------
# SJH 08/07/09 - uses a read in rep file
# SJDM 23/06/2014 - modified so that fisheries with standardised CPUE can be plotted seperately and various other minor shit
#  require(mgcv)

  nfish <- plotrepfile$nFisheries
  year.tmp <- plotrepfile$Rlz.t.fsh
  co.tmp <- plotrepfile$qEdevAtAge
  cp.tmp <- plotrepfile$qAtAge
  no.rows <- ceiling(length(fishplot)/3)
  par(mfrow=c(no.rows,3),mar=c(2,4,1,2)+.1,las=1)
  labs <- fleetlabs

  j <- 0   # Probably a better way to do this but I need a counter for the loop below

  for(i in fishplot) {

  j <- j + 1
  effort <- frqfile$mat[,6][frqfile$mat[,4]==i]
  year <- year.tmp[i,!is.na(year.tmp[i,])]
  cp <- cp.tmp[i,!is.na(cp.tmp[i,])]
  co <- co.tmp[i,!is.na(co.tmp[i,])]
  res <- log(co/cp)
  res[effort==-1] <- NA

    plot(year,res,ann=F,axes=F,lwd=.1,cex=.7,col="slate grey",ylim=ylimit,xlim=c(1950, 2010))
     a <- cbind(year, res)
    a <- a[is.na(a[,2]) == F,]
    a <- a[is.finite(a[,2]) == T,]
    if (length(a[,1])> 60){lines(lowess(a[,1], a[,2], f=0.4),lwd=2)}
    lines(range(year),c(0,0),lwd=.1,lty=2)

    box(bty='l')
    axis(2,lwd=.1)
    if((j-length(fishplot))>-3) axis(1) else axis(1,labels=F)
    mtext(paste("    ",labs[i]),side=3,adj=0,line=0,cex=.7)
  }
}

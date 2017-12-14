plot.q.betyft <- function(parfile=read.par(basepar),plotrepfile=read.rep(baserep),fleetlabs=spp_fleets$fnames){
##-----------------------------------------------------------------------
##-----------------------------------------------------------------------
# SJH 08/07/09 - uses a read in rep file
  nfish <- plotrepfile$nFisheries

  year.tmp <- plotrepfile$Rlz.t.fsh
  co.tmp <- plotrepfile$qAtAge

  std.fish <- parfile$ffl[,10]
  fish.keep <- which(std.fish == 1)

  opar <- par(mfcol=c(ceiling(length(fish.keep)/3),3),mar=c(2,3,1,2))
  on.exit(par(opar))

#  labs <- scan("labels.tmp",what="",sep=":",quiet=T)
  #labs <- 1:25
  labs <- fleetlabs
  
  x=0
  for(i in fish.keep) {
   #if(i==1){browser()}
  x=x+1
   year <- trunc(year.tmp[i,!is.na(year.tmp[i,])])
   co <- aggregate(co.tmp[i,!is.na(co.tmp[i,])],list(year),sum)
   ylims <- range(0,co[,2],na.rm=T)
    plot(co,type='l',ann=F,ylim=ylims,xaxt="n",col="blue",lwd=2,las=1,ylab="Catchability coefficient")#ylim=ylim,
#    xlim=c(1950, 2007))
    #points(co)
#    lines(rep(2003,2),10^(par("usr")[3:4]),lwd=1,lty=3)
    box(bty='l')
    axis(2,las=1)
    if(x==ceiling(length(fish.keep)/3) | x == ceiling(length(fish.keep)/3)*2 | x==length(fish.keep)) axis(1) else axis(1,labels=F)
    mtext(paste("    ",labs[i]),side=3,adj=0,line=0,cex=.7)
  }
}

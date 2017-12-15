plot.growthvar.betyft<- function(frq=tmp.rep,YLIM=c(0,210))
{
# growth curve with variation
nages <- frq$nAges
##length at age
lth <- matrix(frq$mean.LatAge, 1, nages)
##sd at length
lthsd <- matrix(frq$sd.LatAge, 1, nages)
#browser()
lthrange <- seq(from=0,length=max(YLIM))

plot(1:nages, lth, lwd=3, col="black", ylim=YLIM, ylab="", xlab="", type="n", pch=16, xlim=c(0,nages),xaxt="n",las=1)
      for (i in 1:nages){
      a <- dnorm(lthrange, lth[i], lthsd[i])
      #trim the range
      lthrange2 <- lthrange[a > 0.0004]
      a <- a[a > 0.0004]
      a <- a/max(a*1.1)
      #lines(a+i, lthrange)
      polygon(c(a+i, rep(i, length(lthrange2))), c(lthrange2, rev(lthrange2)), col="grey")
      }
lines(1:nages, lth, lwd=3, col="black")
points(1:nages, lth, pch=16, col="black")
############3axis(1, at = seq(0:nages,by=frq$mpy), labels=F)
axis(1, at = seq(0,nages,4),las=1)
mtext(side=1, "Age (quarters)", line=3)
mtext(side=2, "Length (cm)", line=3)
}
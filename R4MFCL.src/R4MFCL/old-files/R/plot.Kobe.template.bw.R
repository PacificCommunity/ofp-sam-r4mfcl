 plot.Kobe.template.bw <-
function(Type){
# SJH 19/01/2009 - makes the base for the Kobe plot
# Based on code by Adam Langley
##plots quadrants
par(mar=c(7,6,2,3))
plot(c(0,0), c(0,0), ylim=c(0,2.2), xlim=c(0,3.5), type="n", ylab="", xlab="")
polygon(c(0,1,1,0,0), c(1,1,2.2,2.2,1), col=NA)
polygon(c(1,3.5,3.5,1,1), c(1,1,2.2,2.2,1), col=NA)
polygon(c(1,3.5,3.5,1,1), c(0,0,1,1,0), col=NA)
polygon(c(0,1,1,0,0), c(0,0,1,1,0), col=NA)
lines(c(0,3.5), rep(1,2), lwd=6)
lines(c(1,1), c(0,2.2), lwd=6)
    if(Type=="SSB")
    {
    mtext(side =1, at=1, "SB=SBmsy", line=2.5, cex=1)
    mtext(side =1, at=0.25, "SB<SBmsy", line=2.5, cex=1)
    mtext(side =1, at=2, "SB>SBmsy", line=2.5, cex=1)
    mtext(side =1, "SB/SBmsy", line=4, cex=1.5)
    } else
    {
    mtext(side =1, at=1, "B=Bmsy", line=2.5, cex=1)
    mtext(side =1, at=0.25, "B<Bmsy", line=2.5, cex=1)
    mtext(side =1, at=2, "B>Bmsy", line=2.5, cex=1)
    mtext(side =1, "B/Bmsy", line=4, cex=1.5)
    }
mtext(side =2, at=1, "F=Fmsy", line=2.5, cex=1)
mtext(side =2, at=0.35, "F<Fmsy", line=2.5, cex=1)
mtext(side =2, at=1.8, "F>Fmsy", line=2.5, cex=1)
mtext(side =2, "F/Fmsy", line=4, cex=1.5)
mtext(side=4, at= 1.5, "Overfishing", line=0.5, cex=1.2)
mtext(side=3, at= 0.5, "Overfished", line=0.5, cex=1.2)
}

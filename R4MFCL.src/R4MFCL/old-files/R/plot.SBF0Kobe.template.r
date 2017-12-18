plot.SBF0Kobe.template <- function(pdims=kplotdim,Green.Zone=green.zone){
# SJH 02/03/2010 - makes the base for the Kobe plot
# Square plot with flexibility on the axes and includes type again

  text1 <- "SB=20%SBF0"
  text2 <- "SB<20%SBF0"
  text3 <- "SB>20%SBF0"
  text4 <- "SB/SBF0"

##plots quadrants
xmax <- pdims[1]
ymax <- pdims[2]

over.loc <- c(mean(c(pdims[1],1)),mean(c(pdims[2],1)))
#browser()

par(mar=c(7,6,2,3),pty="s")
plot(c(0,0), c(0,0), ylim=c(0,ymax), xlim=c(0,xmax), axes=F,type="n", ylab="", xlab="")
axis(1)
axis(2,las=1)
polygon(c(0,0.2,0.2,0,0), c(0,0,ymax,ymax,0), col="red",border=NA)
polygon(c(0.2,xmax,xmax,0.2,0.2), c(1,1,ymax,ymax,1), col="orange",border=NA)
#polygon(c(1,xmax,xmax,1,1), c(0,0,1,1,0), col="green")
#polygon(c(0,1,1,0,0), c(0,0,1,1,0), col="yellow")
# possible zone for TRP
#polygon(x=c(0.4,0.6,0.6,0.4,0.4),y=c(0,0,1,1,0),col="green",density=c(10,20))
if(!is.null(Green.Zone)) polygon(x=Green.Zone$x,y=Green.Zone$y,col=Green.Zone$col,density=Green.Zone$density)
#border for red zone
segments(0.2,0,0.2,ymax,lwd=3)
segments(0,1,xmax,1, lwd=3,lty=4)
#segments(sbmsy,0,sbmsy,1,lty=4,col="grey", lwd=2)

mtext(side =1, at=0.2, text1, line=2.5, cex=.7)
mtext(side =1, at=0.01, text2, line=2.5, cex=.7)
mtext(side =1, at=0.6, text3, line=2.5, cex=.7)
mtext(side =1, text4, line=4, cex=1.5)
mtext(side =2, at=1, "F=Fmsy", line=2.5, cex=.7)
mtext(side =2, at=0.35, "F<Fmsy", line=2.5, cex=.7)
mtext(side =2, at=over.loc[2], "F>Fmsy", line=2.5, cex=.7)
mtext(side =2, "F/Fmsy", line=4, cex=1.5)
}

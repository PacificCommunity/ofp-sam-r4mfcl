plot.Kobe.template.sjh <- function(pdims=kplotdim,Type=type,FRENCH=French){
# SJH 02/03/2010 - makes the base for the Kobe plot
# Square plot with flexibility on the axes and includes type again

  if(Type=="SSB")
  {
  text1 <- "SB=SBmsy"
  text2 <- "SB<SBmsy"
  text3 <- "SB>SBmsy"
  text4 <- "SB/SBmsy"
  } else{
  text1 <- "B=Bmsy"
  text2 <- "B<Bmsy"
  text3 <- "B>Bmsy"
  text4 <- "B/Bmsy"
  }
##plots quadrants
xmax <- pdims[1]
ymax <- pdims[2]

over.loc <- c(mean(c(pdims[1],1)),mean(c(pdims[2],1)))
#browser()

par(mar=c(7,6,2,3),pty="s")
plot(c(0,0), c(0,0), ylim=c(0,ymax), xlim=c(0,xmax), axes=F,type="n", ylab="", xlab="")
axis(1)
axis(2,las=1)
polygon(c(0,1,1,0,0), c(1,1,ymax,ymax,1), col="red")
polygon(c(1,xmax,xmax,1,1), c(1,1,ymax,ymax,1), col="orange")
polygon(c(1,xmax,xmax,1,1), c(0,0,1,1,0), col="green")
polygon(c(0,1,1,0,0), c(0,0,1,1,0), col="yellow")
lines(c(0,xmax), rep(1,2), lwd=6)
lines(c(1,1), c(0,ymax), lwd=6)
mtext(side =1, at=1, text1, line=2.5, cex=.8)
mtext(side =1, at=0.05, text2, line=2.5, cex=.8)
mtext(side =1, at=over.loc[1], text3, line=2.5, cex=.8)
mtext(side =1, text4, line=4, cex=1.5)
mtext(side =2, at=1, "F=Fmsy", line=2.5, cex=.8)
mtext(side =2, at=0.35, "F<Fmsy", line=2.5, cex=.8)
mtext(side =2, at=over.loc[2], "F>Fmsy", line=2.5, cex=.8)
mtext(side =2, "F/Fmsy", line=4, cex=1.5)

if(FRENCH)
{
mtext(side=4, at= 1.5, "Surp\u00e9che", line=0.5, cex=1.2)
mtext(side=3, at= 0.5, "Surp\u00e9ch\u00ea", line=0.5, cex=1.2)
}else{
mtext(side=4, at= 1.5, "Overfishing", line=0.5, cex=1.2)
mtext(side=3, at= 0.5, "Overfished", line=0.5, cex=1.2)
}
}

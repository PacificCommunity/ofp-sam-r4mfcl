 plot.pacific.yft <-
function(plot_title="",lims=c(100,260,-45,45),add.WCPFC=F) {
# By Adam Langley 
# Modified by Simon Hoyle
# need these libraries
#library(maps)
#library(mapproj)
#library(mapdata)
eez <- read.table("I:/assessments/Pop dy modeling/MFCL/R functions/EZNEW2.TXT")
plot(1,1, yaxt="n", xaxt="n", type="n", xlim=c(lims[1]+10,lims[2]-10), ylim=c(lims[3]+5,lims[4]-5), ylab="", xlab="", bg="lightblue")
polygon(c(lims[1],lims[2],lims[2],lims[1]), c(lims[3],lims[3],lims[4],lims[4]), col="lightblue")
polygon(eez[,1], eez[,2], lwd=1, col="white")
lines(eez[,1], eez[,2], lwd=1, col="slate grey")
map('world2Hires',  yaxt="n", xaxt="n", add=T, resolution=1)
map('world2Hires',  region = c("USA","Hawaii","Mexico","Japan","China","South Korea","North Korea","Philippines","Vietnam","Laos","Taiwan","Fiji", "Vanuatu", "Malaysia", "Australia", "New Zealand", "Indonesia", "New Caledonia", "Papua New Guinea", "Solomon Islands"), fill=T, add=T, yaxt="n", xaxt="n", col="black", density=50)
box(lwd=3)
if(add.WCPFC) { lines(c(210, 210, 230, 230), c(45, -2.5, -2.5, -45), lwd=2, lty=2) }
lines(c(210, 210), c(40, -40), lwd=2, lty=1)
lines(c(170, 170), c(-40, 40), lwd=2, lty=1)
lines(c(120, 210), c(20, 20), lwd=2, lty=1)
lines(c(120, 210), c(40, 40), lwd=2, lty=1)
lines(c(120, 210), c(-10, -10), lwd=2, lty=1)
lines(c(120, 210), c(-40, -40), lwd=2, lty=1)
axis(1, at=seq(lims[1],lims[2],by=10), labels=F)
axis(2, at=seq(lims[3],lims[4],by=5), labels=F)
latseq <- seq(lims[3]+10,lims[4]-10,by=10) ;latseq2 <- as.character(latseq) 
lonseq <- seq(lims[1]+20,lims[2]-20,by=20) ;lonseq2 <- as.character(lonseq) 
latseq2[latseq < 0] <- paste(abs(latseq[latseq < 0]),"S",sep="")
latseq2[latseq > 0] <- paste(latseq[latseq > 0],"N",sep="")
lonseq2[lonseq < 180] <- paste(lonseq2[lonseq < 180],"E",sep="")
lonseq2[lonseq > 180] <- paste(360-lonseq[lonseq > 180],"W",sep="")
axis(2, at=latseq, labels=latseq2, cex.axis=0.75)
axis(1, at=lonseq, labels=lonseq2, cex.axis=0.75)
mtext(side=3, line=0.5, plot_title)
}

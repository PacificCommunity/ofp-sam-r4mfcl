 plot.pacific.species <-
function(plot_title="",uselims=NA,add.WCPFC=F,add.EPO=F,sp="YFT",add.EEZ=T,eez_file="I:/assessments/Pop dy modeling/MFCL/R functions/EZNEW2.TXT") {
# Original by Adam Langley, modified by Simon Hoyle
# need these libraries
#library(maps)
#library(mapproj)
#library(mapdata)
if (add.EEZ) eez <- read.table(eez_file) 
if(sp=="SKJold") { lims=c(100,230,-30,50) }
if(sp=="SKJnew" | sp=="SKJ") { lims=c(100,230,-30,50) }
if(sp=="YFT" | sp=="BET") { lims=c(100,260,-45,45) }
if(sp=="ALB") { lims=c(120,295,-60,10) }
if(is.na(uselims)==F) { lims <- uselims }
if(add.EPO==T) { lims[2] <- 295 }
plot(1,1, yaxt="n", xaxt="n", type="n", xlim=c(lims[1]+10,lims[2]-10), ylim=c(lims[3]+5,lims[4]-5), ylab="", xlab="", bg="lightblue")
polygon(c(lims[1],lims[2],lims[2],lims[1]), c(lims[3],lims[3],lims[4],lims[4]), col="lightblue")
if(add.EEZ) { 
  polygon(eez[,1], eez[,2], lwd=1, col="white")
  lines(eez[,1], eez[,2], lwd=1, col="slate grey")
  }
map('world2Hires',  yaxt="n", xaxt="n", add=T, resolution=1)
map('world2Hires',  region = c("USA","Hawaii","Mexico","Japan","China","South Korea","North Korea","Philippines","Vietnam","Laos","Taiwan","Fiji", "Vanuatu", "Malaysia","Burma","Thailand","Cambodia",
    "Australia", "New Zealand", "Indonesia", "New Caledonia", "Papua New Guinea", "Solomon Islands","Mongolia"), 
    fill=T, add=T, yaxt="n", xaxt="n", col="black", density=50)
if (add.EPO==T | sp=="ALB") { 
  map('world2Hires',  region = c("Colombia","Chile","Peru","Ecuador","Argentina","Honduras",
    "Nicaragua","Guatemala","Belize","Brazil","Costa Rica","Panama","El Salvador","Cuba","Venezuela","Bolivia","Paraguay","Haiti","Dominican Republic","Jamaica","Canada"), 
    fill=T, add=T, yaxt="n", xaxt="n", col="black", density=50)
    }
box(lwd=3)
if(add.WCPFC) { lines(c(210, 210, 230, 230), c(45, -2.5, -2.5, -45), lwd=2, lty=2) }
if(sp=="YFT" | sp=="BET") {
lines(c(210, 210), c(40, -40), lwd=2, lty=1)
lines(c(170, 170), c(-40, 40), lwd=2, lty=1)
lines(c(120, 210), c(20, 20), lwd=2, lty=1)
lines(c(120, 210), c(40, 40), lwd=2, lty=1)
lines(c(120, 210), c(-10, -10), lwd=2, lty=1)
lines(c(120, 210), c(-40, -40), lwd=2, lty=1)
text(168,22,labels=1,font=2,cex=1.2)
text(208,22,labels=2,font=2,cex=1.2)
text(168,-8,labels=3,font=2,cex=1.2)
text(208,-8,labels=4,font=2,cex=1.2)
text(168,-38,labels=5,font=2,cex=1.2)
text(208,-38,labels=6,font=2,cex=1.2)
}
if(sp=="SKJold") {
lines(c(140,140,210), c(25,45,45), lwd=2,lty=1)
lines(c(120,120,140), c(25,35,35), lwd=2,lty=1)
lines(c(165,165), c(25,45), lwd=2,lty=1)
lines(c(115,210), c(25,25), lwd=2,lty=1)
lines(c(115,210), c(15,15), lwd=2,lty=1)
lines(c(115,115,142.5,142.5,210), c(25,-10,-10,-20,-20), lwd=2,lty=1)
lines(c(165,165), c(15,-20), lwd=2,lty=1)
lines(c(210,210), c(45,-20), lwd=2,lty=1)
text(138,27,labels=1,font=2,cex=1.2)
text(163,27,labels=2,font=2,cex=1.2)
text(208,27,labels=3,font=2,cex=1.2)
text(208,18,labels=4,font=2,cex=1.2)
text(163,-18,labels=5,font=2,cex=1.2)
text(208,-18,labels=6,font=2,cex=1.2)
}
if(sp=="SKJnew" | sp=="SKJ") {
lines(c(115,210), c(20,20), lwd=2,lty=1)
lines(c(115,115,210,210,115), c(-20,45,45,-20,-20), lwd=2,lty=1)
lines(c(170,170), c(20,-20), lwd=2,lty=1)
lines(c(115,115), c(20,-20), lwd=2,lty=1)
text(208,22,labels=1,font=2,cex=1.2)
text(168,-18,labels=2,font=2,cex=1.2)
text(208,-18,labels=3,font=2,cex=1.2)
}
if(sp=="ALB") {
  lines(c(125,290), c(0,0), lwd=3, col="slate grey", lty=1)
  lines(c(180,180), c(-50,0), lwd=3, col="slate grey", lty=1)
  lines(c(250,250), c(-50,0), lwd=3, col="slate grey", lty=1)
  lines(c(155,290), c(-25,-25), lwd=3, col="slate grey", lty=1)
  xoffset <- 5
  yoffset <- 2.5
  text(145+xoffset,0-yoffset,"R1", col="red", cex=1.5)
  text(240+xoffset,0-yoffset,"R2", col="red",cex=1.5)
  text(143+xoffset,-25-yoffset,"R3", col="red",cex=1.5)
  text(240+xoffset,-25-yoffset,"R4", col="red",cex=1.5)
  text(250+xoffset,0-yoffset,"R5", col="red",cex=1.5)
  text(250+xoffset,-25-yoffset,"R6", col="red",cex=1.5)
}
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
mtext(side=3, line=0.5, plot_title,font=2,cex=1.1)
}

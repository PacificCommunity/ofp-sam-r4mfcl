 plot.pacific.alb <-
function(plot_title="",eez_dir="I:/assessments/Pop dy modeling/MFCL/R functions/",plot_eez=T) {
  # By Adam Langley
  # Modified by Simon Hoyle
  # need these libraries
  #library(maps)
  #library(mapproj)
  #library(mapdata)
  eez <- read.table(paste(eez_dir,"eznew2.txt",sep="/"))
  plot(1,1, yaxt="n", xaxt="n", type="n", xlim=c(130,285), ylim=c(-45,0), ylab="", xlab="", bg="lightblue")
  polygon(c(120, 300, 300, 120), c(-50, -50, 5, 5), col="lightblue")
  box(lwd=3)
  axis(1, at=c(120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280), labels=F)
  axis(2, at=c(-45,-40,-35,-30,-25,-20,-15,-10,-5,0), labels=F)
  axis(2, at=c(-45,-35,-25,-15, -5), labels=c("45S","35S","25S", "15S","5S"), cex.axis=1.2)
  axis(1, at=c(140,160,180,200,220,240,260,280), labels=c("140E", "160E", "180", "160W", "140W", "120W","100W", "80W"), cex.axis=1.2)
  if(plot_eez) {
    lines(eez[,1], eez[,2], lwd=1, col="slate grey")
    polygon(eez[,1], eez[,2], lwd=1, col="white")
  }
  map('world2Hires',  yaxt="n", xaxt="n", add=T, resolution=1)
  map('world2Hires',  region = c("Fiji", "Vanuatu", "Malaysia", "Australia", "New Zealand", "Indonesia", "New Caledonia", "Papua New Guinea", "Solomon Islands",
        "Peru", "Colombia", "Mexico", "Chile","Ecuador","Argentina","Brazil"), fill=T, add=T, yaxt="n", xaxt="n", col="black", density=50)
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

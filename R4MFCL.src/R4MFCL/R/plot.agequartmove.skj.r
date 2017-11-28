#' @import mapdata
plot.agequartmove.skj <- function(repfile=baserep,parfile=basepar)
{
##plot movement with arrows of length proportional to percentage movement
##arrows
##access diffusion coefficents
##proportion of fish in region after movement
##for BET WCPO
##the blocks are region*region (4*4)
# 1>1 2>1 3>1 4>1 5>1 6>1 7>1 8>1
# 1>2 2>2 3>2 4>2
# 1>3 2>3 3>3 4>3
# 1>4 2>4 3>4 4>4
# 1>5............................
# 1>6............................
#require(mapdata)
#require(mapplots)
#require(mapproj)


#by age class (40)
#by quarter (4)
jnk <- read.rep(repfile)


regions <- jnk$nReg
nage <- jnk$nAges
qtrs <- jnk$nRecs.yr # should use mpy from frq file!
linesget <- regions*nage*qtrs
# movement coefficients
pos1 <- grep("# movement matrices",readLines(parfile))

mat <- as.matrix(read.table(parfile, skip=pos1, nrows= linesget))
matq <- array(NA, dim=c(regions*nage, regions, qtrs))
a <- regions*nage
matq[,,1] <- mat[1:a,1:regions]
matq[,,2] <- mat[(a+1):(2*a),1:regions]
matq[,,3] <- mat[(2*a+1):(3*a),1:regions]
matq[,,4] <- mat[(3*a+1):(4*a),1:regions]
assign("movements",matq,pos=1)

#par(mar=c(4,5,2,2))
#map('world2Hires', xlim=c(110, 220), ylim=c(-50,50))

par(mfrow=c(2,2), mar=c(1,1,1,1), omi=c(0,0,0.1,0))
for(j in 1:4){
#j <- 1
#map('world2Hires', xlim=c(110, 220), ylim=c(-40,40))
plot(c(0:4), c(0:4), type="n", yaxt="n", xaxt="n", ylab="", xlab="", bty="n",ylim=c(-30,55), xlim=c(100,220))
map('world2Hires', xlim=c(100, 220), ylim=c(-30,55), add=T, col="light grey")
box(lwd=3)
#polygon(c(0,4,4,0,0), c(0,0,5,5,0), lwd=3)
lines(c(110,210), c(20,20), lwd=3, col="slate grey", lty=1)
lines(c(110,210), c(-20,-20), lwd=3, col="slate grey", lty=1)
lines(c(120,210), c(50,50), lwd=3, col="slate grey", lty=1)
lines(c(110,110), c(-20,20), lwd=3, col="slate grey", lty=1)
lines(c(140,140), c(-20,20), lwd=3, col="slate grey", lty=1)
lines(c(170,170), c(-20,20), lwd=3, col="slate grey", lty=1)
lines(c(210,210), c(-20,50), lwd=3, col="slate grey", lty=1)
lines(c(120,120), c(20,50), lwd=3, col="slate grey", lty=1)

# Region 5 boundaries
lines(c(140,155), c(0,0), lwd=3, col="slate grey", lty=1)
lines(c(155,155), c(0,-5), lwd=3, col="slate grey", lty=1)
lines(c(155,160), c(-5,-5), lwd=3, col="slate grey", lty=1)
lines(c(160,160), c(-5,-20), lwd=3, col="slate grey", lty=1)

xoffset <- 5
yoffset <- 1

#age classes 1, 10, 20, 30
ages <- c(1, 4, 8, 12)*regions - (regions-1)
##distance between arrows on x and y axis
yincr <- 1.7
xincr <- 2.0 # formerly 2.5
## rescale based on dimensions of plot so that x and y areas proportional
minlat <- -30
maxlat <- 55
minlong <- 100
maxlong <- 220
scaley <- 40 * (maxlat-minlat)/(maxlong-minlong)
scalex <- 40

#1>2
for (i in 1:length(ages)){
r2 <- 1
r1 <- 2 + ages[i] - 1
x <-  155-xincr*i
y <- scaley * matq[r1,r2,j] * 0.5
arrows(x, 20+y, x,20-y, angle=10, length=0.1,lwd=i)
}
#2>1
for (i in 1:length(ages)){
r2 <- 2
r1 <- 1 + ages[i] - 1
x <-  155+xincr*i
y <- scaley * matq[r1,r2,j] * 0.5
arrows(x, 20-y, x,20+y, angle=10, length=0.1,lwd=i)
}

#1>3
for (i in 1:length(ages)){
r2 <- 1
r1 <- 3 + ages[i] - 1
x <-  190-xincr*i
y <- scaley * matq[r1,r2,j] * 0.5
arrows(x, 20+y, x,20-y, angle=10, length=0.1,lwd=i)
}
#3>1
for (i in 1:length(ages)){
r2 <- 3
r1 <- 1 + ages[i] - 1
x <-  190+xincr*i
y <- scaley * matq[r1,r2,j] * 0.5
arrows(x, 20-y, x,20+y, angle=10, length=0.1,lwd=i)
}

#1>4
for (i in 1:length(ages)){
r2 <- 1
r1 <- 4 + ages[i] - 1
x <-  125-xincr*i
y <- scaley * matq[r1,r2,j] * 0.5
arrows(x, 20+y, x,20-y, angle=10, length=0.1,lwd=i)
}
#4>1
for (i in 1:length(ages)){
r2 <- 4
r1 <- 1 + ages[i] - 1
x <-  125+xincr*i
y <- scaley * matq[r1,r2,j] * 0.5
arrows(x, 20-y, x,20+y, angle=10, length=0.1,lwd=i)
}

#2>5
for (i in 1:length(ages)){
r2 <- 2
r1 <- 5 + ages[i] - 1
x <-  150-xincr*i
y <- scaley * matq[r1,r2,j] * 0.5
arrows(x, 0+y, x,0-y, angle=10, length=0.1,lwd=i)
}
#5>2
for (i in 1:length(ages)){
r2 <- 5
r1 <- 2 + ages[i] - 1
x <-  150+xincr*i
y <- scaley * matq[r1,r2,j] * 0.5
arrows(x, 0-y, x,0+y, angle=10, length=0.1,lwd=i)
}

#2>3
for (i in 1:length(ages)){
r2 <- 2
r1 <- 3 + ages[i] - 1
y <-  7-yincr*i
x <- scalex * matq[r1,r2,j] * 0.5
arrows(170-x, y, 170+x,y, angle=10, length=0.1,lwd=i)
}
#3>2
for (i in 1:length(ages)){
r2 <- 3
r1 <- 2 + ages[i] - 1
y <-  7+yincr*i
x <- scalex * matq[r1,r2,j] * 0.5
arrows(170+x, y, 170-x,y, angle=10, length=0.1,lwd=i)
}

#4>5
for (i in 1:length(ages)){
r2 <- 4
r1 <- 5 + ages[i] - 1
y <-  -12-yincr*i
x <- scalex * matq[r1,r2,j] * 0.5
arrows(140-x, y, 140+x,y, angle=10, length=0.1,lwd=i)
}
#5>4
for (i in 1:length(ages)){
r2 <- 5
r1 <- 4 + ages[i] - 1
y <-  -12+yincr*i
x <- scalex * matq[r1,r2,j] * 0.5
arrows(140+x, y, 140-x,y, angle=10, length=0.1,lwd=i)
}

#2>4
for (i in 1:length(ages)){
r2 <- 2
r1 <- 4 + ages[i] - 1
y <-  10-yincr*i
x <- scalex * matq[r1,r2,j] * 0.5
arrows(140-x, y, 140+x,y, angle=10, length=0.1,lwd=i)
}
#4>2
for (i in 1:length(ages)){
r2 <- 4
r1 <- 2 + ages[i] - 1
y <-  10+yincr*i
x <- scalex * matq[r1,r2,j] * 0.5
arrows(140+x, y, 140-x,y, angle=10, length=0.1,lwd=i)
}

text(170+xoffset,30-yoffset,"R1", cex=1, col="red")
text(150+xoffset,10-yoffset,"R2", cex=1, col="red")
text(190+xoffset,0-yoffset,"R3", cex=1, col="red")
text(120+xoffset,0-yoffset,"R4", cex=1, col="red")
text(145+xoffset,-10+yoffset,"R5", cex=1, col="red")

mtext(side=3, paste("Quarter", j), line=0.2)
}
}

